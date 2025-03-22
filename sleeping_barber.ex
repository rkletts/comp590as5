defmodule Customer do
  def new(receptionist, id) do
    spawn_link(fn -> init(receptionist, id) end)
  end

  def init(receptionist, id) do
    Receptionist.greet(receptionist, self(), id)
    await_response(id)
  end

  defp await_response(id) do
    receive do
      {:wait, _waiting_room, barber} ->
        wait_for_haircut(barber, id)
      :shop_full ->
        IO.puts("Customer #{id} leaves - shop full")
        exit(:normal)
    end
  end

  defp wait_for_haircut(_barber, id) do
    receive do
      :haircut_done ->
        IO.puts("Customer #{id} got haircut and leaves happy")
        exit(:normal)
    end
  end
end

defmodule Barber do
  use GenServer

  # Hot-swappable behavior module
  defmodule Behavior do
    def cut_hair do
      time = :rand.uniform(4000) + 2000
      IO.puts("Cutting hair for #{time}ms")
      Process.sleep(time)
    end
  end

  def start_link do
    GenServer.start_link(__MODULE__, :sleeping)
  end

  # Client API
  def cut_hair(barber, customer, id) do
    GenServer.call(barber, {:cut_hair, customer, id}, 10_000)
  end

  # Server callbacks
  def init(:sleeping) do
    IO.puts("Barber is sleeping")
    {:ok, :sleeping}
  end

  def handle_call({:cut_hair, customer, id}, _from, state) do
    # Load latest behavior module
    :code.purge(Behavior)
    :code.load_file(Behavior)
    
    IO.puts("Barber cutting hair for Customer #{id}")
    Behavior.cut_hair()
    send(customer, :haircut_done)
    
    {:reply, :ok, state}
  end
end

defmodule WaitingRoom do
  use GenServer

  def start_link(capacity) do
    GenServer.start_link(__MODULE__, {capacity, :queue.new(), nil})
  end

  # Client API
  def enter(room, customer, id) do
    GenServer.call(room, {:enter, customer, id})
  end

  def next_customer(room) do
    GenServer.call(room, :next_customer)
  end

  def customer_in_service(room, id) do
    GenServer.cast(room, {:in_service, id})
  end

  def service_complete(room) do
    GenServer.cast(room, :service_complete)
  end

  # Server callbacks
  def init({capacity, queue, current}) do
    IO.puts("Waiting room started with #{capacity} chairs")
    {:ok, {capacity, queue, current}}
  end

  def handle_call({:enter, customer, id}, _from, {capacity, queue, current}) do
    waiting_count = :queue.len(queue)
    
    if waiting_count < capacity do
      IO.puts("Customer #{id} enters waiting room (#{waiting_count + 1}/#{capacity} seats taken)")
      {:reply, :ok, {capacity, :queue.in({customer, id}, queue), current}}
    else
      IO.puts("Waiting room full (#{waiting_count}/#{capacity} seats taken), turning away Customer #{id}")
      {:reply, :full, {capacity, queue, current}}
    end
  end

  def handle_call(:next_customer, _from, {capacity, queue, _current}) do
    case :queue.out(queue) do
      {{:value, {customer, id}}, new_queue} ->
        {:reply, {:ok, customer, id}, {capacity, new_queue, id}}
      {:empty, queue} ->
        {:reply, :empty, {capacity, queue, nil}}
    end
  end

  def handle_cast({:in_service, id}, {capacity, queue, _current}) do
    {:noreply, {capacity, queue, id}}
  end

  def handle_cast(:service_complete, {capacity, queue, _current}) do
    {:noreply, {capacity, queue, nil}}
  end
end

defmodule Receptionist do
  use GenServer

  def start_link(waiting_room, barber) do
    GenServer.start_link(__MODULE__, {waiting_room, barber})
  end

  # Client API
  def greet(receptionist, customer, id) do
    GenServer.cast(receptionist, {:greet, customer, id})
  end

  # Server callbacks
  def init({waiting_room, barber}) do
    IO.puts("Receptionist started")
    {:ok, {waiting_room, barber}}
  end

  def handle_cast({:greet, customer, id}, {waiting_room, barber} = state) do
    IO.puts("Receptionist greeting Customer #{id}")
    case WaitingRoom.enter(waiting_room, customer, id) do
      :ok ->
        send(customer, {:wait, waiting_room, barber})
      :full ->
        send(customer, :shop_full)
    end
    {:noreply, state}
  end
end

defmodule BarberShop do
  def start do
    Process.flag(:trap_exit, true)  # Trap exits to prevent crashes
    
    # Start all the processes
    {:ok, waiting_room} = WaitingRoom.start_link(6)  # 6 chairs
    {:ok, barber} = Barber.start_link()
    {:ok, receptionist} = Receptionist.start_link(waiting_room, barber)
    
    # Start customer generator and service loop with restart capability
    spawn_monitor(fn -> generate_customers(receptionist, 1) end)
    spawn_monitor(fn -> service_loop(waiting_room, barber) end)
    
    # Monitor the processes
    monitor_processes(waiting_room, barber, receptionist)
  end

  defp generate_customers(receptionist, customer_id) do
    try do
      Process.sleep(:rand.uniform(5000))
      :code.purge(Customer)
      :code.load_file(Customer)
      Customer.new(receptionist, customer_id)
      generate_customers(receptionist, customer_id + 1)
    rescue
      _ -> 
        IO.puts("Customer generator recovering...")
        generate_customers(receptionist, customer_id)
    catch
      _ ->
        IO.puts("Customer generator recovering from catch...")
        generate_customers(receptionist, customer_id)
    end
  end

  defp service_loop(waiting_room, barber) do
    try do
      case WaitingRoom.next_customer(waiting_room) do
        {:ok, customer, id} ->
          try do
            WaitingRoom.customer_in_service(waiting_room, id)
            Barber.cut_hair(barber, customer, id)
            WaitingRoom.service_complete(waiting_room)
          catch
            :exit, {:timeout, _} ->
              IO.puts("Haircut timeout for Customer #{id}, continuing...")
              WaitingRoom.service_complete(waiting_room)
          end
        :empty ->
          Process.sleep(1000)  # Sleep if no customers
      end
      service_loop(waiting_room, barber)
    rescue
      _ -> 
        IO.puts("Service loop recovering...")
        Process.sleep(1000)
        service_loop(waiting_room, barber)
    catch
      _ ->
        IO.puts("Service loop recovering from catch...")
        Process.sleep(1000)
        service_loop(waiting_room, barber)
    end
  end

  defp monitor_processes(waiting_room, barber, receptionist) do
    receive do
      {:DOWN, _, :process, _pid, _reason} ->
        IO.puts("A process died, restarting the system...")
        start()
      _ ->
        monitor_processes(waiting_room, barber, receptionist)
    end
  end
end

# Start the barber shop
BarberShop.start()

# Keep the script running forever
Process.sleep(:infinity)