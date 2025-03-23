defmodule BarberBehavior do
  def cut_hair do
    time = :rand.uniform(4000) + 2000
    IO.puts("Cutting hair for #{time}ms")
    Process.sleep(time)
  end
end

defmodule CustomerBehavior do
  def wait_for_haircut(barber, id) do
    receive do
      :haircut_done ->
        IO.puts("Customer #{id} got haircut and leaves happy")
        exit(:normal)
    end
  end
end


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
        Code.ensure_loaded(CustomerBehavior)
        CustomerBehavior.wait_for_haircut(barber, id)
      :shop_full ->
        IO.puts("Customer #{id} leaves - shop is full")
        exit(:normal)
    end
  end
end


defmodule Barber do
  use GenServer


  def start_link do
    GenServer.start_link(__MODULE__, :sleeping)
  end


  def cut_hair(barber, customer, id) do
    try do
      GenServer.call(barber, {:cut_hair, customer, id}, :infinity)
    catch
      :exit, _ ->
        IO.puts("Retrying haircut for Customer #{id}")
        Process.sleep(1000)
        cut_hair(barber, customer, id)
    end
  end


  def init(:sleeping) do
    IO.puts("Barber is sleeping")
    {:ok, :sleeping}
  end


  def handle_call({:cut_hair, customer, id}, _from, state) do
    Code.ensure_loaded(BarberBehavior)
   
    IO.puts("Barber cutting hair for Customer #{id}")
    try do
      BarberBehavior.cut_hair()
      send(customer, :haircut_done)
      {:reply, :ok, state}
    catch
      kind, reason ->
        IO.puts("Error during haircut: #{inspect(kind)} #{inspect(reason)}")
        {:reply, :error, state}
    end
  end
end


defmodule WaitingRoom do
  use GenServer

  def start_link(capacity) do
    GenServer.start_link(__MODULE__, {capacity, :queue.new(), nil})
  end

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

  def greet(receptionist, customer, id) do
    GenServer.cast(receptionist, {:greet, customer, id})
  end

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
    Process.flag(:trap_exit, true)

    {:ok, waiting_room} = WaitingRoom.start_link(6)
    {:ok, barber} = Barber.start_link()
    {:ok, receptionist} = Receptionist.start_link(waiting_room, barber)

    spawn_monitor(fn -> generate_customers(receptionist, 1) end)
    spawn_monitor(fn -> service_loop(waiting_room, barber) end)
  end

  defp generate_customers(receptionist, customer_id) do
    Process.sleep(:rand.uniform(5000))

    Code.ensure_loaded(Customer)
    Customer.new(receptionist, customer_id)

    generate_customers(receptionist, customer_id + 1)
  end

   defp service_loop(waiting_room, barber) do
    case WaitingRoom.next_customer(waiting_room) do
      {:ok, customer, id} ->
        WaitingRoom.customer_in_service(waiting_room, id)
        try do
          Barber.cut_hair(barber, customer, id)
          WaitingRoom.service_complete(waiting_room)
        catch
          kind, reason ->
            IO.puts("Error in service loop: #{inspect(kind)} #{inspect(reason)}")
        end
      :empty ->
        IO.puts("No customers, barber is sleeping...")
        Process.sleep(1000)
    end


    service_loop(waiting_room, barber)
  end



  def keep_alive do
    Process.sleep(1000)
    keep_alive()
  end
end

# Start the barbershop without blocking IEx
spawn(fn -> BarberShop.start() end)
spawn(fn -> BarberShop.keep_alive() end)
