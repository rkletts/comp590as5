defmodule SleepingBarber do
  def start do
    barber = spawn_link(__MODULE__, :barber_loop, [])
    waiting_room = spawn_link(__MODULE__, :waiting_room_loop, [[], 6, barber])
    receptionist = spawn_link(__MODULE__, :receptionist_loop, [waiting_room])
    spawn(__MODULE__, :customer_generator, [receptionist])
  end

  def barber_loop do
    receive do
      {:next_customer, customer} ->
        IO.puts("Barber is cutting hair...")
        :timer.sleep(:rand.uniform(3000))
        IO.puts("Barber finished haircut.")
        send(customer, :done)
        barber_loop()
      :no_customers ->
        IO.puts("Barber is taking a short break.")
        :timer.sleep(:rand.uniform(2000))
        barber_loop()
    end
  end

  def waiting_room_loop(queue, max_size, barber) do
    receive do
      {:new_customer, customer} when length(queue) < max_size ->
        IO.puts("Customer takes a seat in the waiting room.")
        new_queue = queue ++ [customer]
        send(barber, {:next_customer, hd(new_queue)})
        waiting_room_loop(tl(new_queue), max_size, barber)
      {:new_customer, _customer} ->
        IO.puts("Waiting room is full. Customer leaves.")
        waiting_room_loop(queue, max_size, barber)
      {:barber_done} ->
        if queue == [] do
          send(barber, :no_customers)
        else
          send(barber, {:next_customer, hd(queue)})
        end
        waiting_room_loop(tl(queue), max_size, barber)
    end
  end

  def receptionist_loop(waiting_room) do
    receive do
      {:new_customer, customer} ->
        IO.puts("Receptionist greets a new customer.")
        send(waiting_room, {:new_customer, customer})
        receptionist_loop(waiting_room)
    end
  end

  def customer_generator(receptionist) do
    spawn(fn ->
      customer = self()
      send(receptionist, {:new_customer, customer})
      receive do
        :done -> IO.puts("Customer leaves after haircut.")
      end
    end)
    :timer.sleep(:rand.uniform(2000))
    customer_generator(receptionist)
  end
end
