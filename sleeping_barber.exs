defmodule BarberShop do
  def start do
    IO.puts("Barber shop is opening!")

    barber = spawn_link(__MODULE__, :barber_loop, [self(), 1])  # Barber process
    waiting_room = spawn_link(__MODULE__, :waiting_room_loop, [[]])  # Waiting room process
    receptionist = spawn_link(__MODULE__, :receptionist_loop, [waiting_room, barber])  # Receptionist process

    spawn(__MODULE__, :customer_generator, [receptionist])

    # Keep the main process alive
    loop_forever()
  end

  def barber_loop(shop, chairs) do
    receive do
      {:cut_hair, customer} ->
        IO.puts("Barber is cutting hair...")
        :timer.sleep(:rand.uniform(3000))  # Simulate cutting time
        IO.puts("Barber finished haircut.")
        send(customer, :haircut_done)
        send(shop, :barber_ready)  # Tell the shop that the barber is ready
        barber_loop(shop, chairs)

      {:update_barber_behavior, new_function} ->
        IO.puts("Updating barber behavior...")
        barber_loop(new_function, chairs)

      {:take_break} ->
        IO.puts("Barber is taking a break...")
        :timer.sleep(5000)  # Simulate break time
        IO.puts("Barber is back to work.")
        barber_loop(shop, chairs)

      {:add_chair} ->
        IO.puts("Barber shop is adding another chair!")
        barber_loop(shop, chairs + 1)
    end
  end

  def waiting_room_loop(queue) do
    receive do
      {:enter, customer, _receptionist} when length(queue) < 6 ->
        IO.puts("Customer enters waiting room.")
        waiting_room_loop(queue ++ [customer])

      {:enter, _, _} ->
        IO.puts("Waiting room full. Customer leaves.")
        waiting_room_loop(queue)

      {:next_customer, barber} ->
        case queue do
          [next | rest] ->
            IO.puts("Customer goes to barber chair.")
            send(barber, {:cut_hair, next})
            waiting_room_loop(rest)
          [] ->
            waiting_room_loop(queue)
        end
    end
  end

  def receptionist_loop(waiting_room, barber) do
    receive do
      {:new_customer, customer} ->
        IO.puts("Receptionist greets a new customer.")
        send(waiting_room, {:enter, customer, self()})
        send(waiting_room, {:next_customer, barber})  # **This line ensures customers get to the barber**

      :barber_ready ->
        send(waiting_room, {:next_customer, barber})
    end

    receptionist_loop(waiting_room, barber)
  end

  def customer_generator(receptionist) do
    IO.puts("Generating a new customer...")
    :timer.sleep(:rand.uniform(2000))

    customer = spawn(__MODULE__, :customer_loop, [])
    IO.puts("New customer arrived!")

    send(receptionist, {:new_customer, customer})
    customer_generator(receptionist)
  end

  def customer_loop do
    receive do
      :haircut_done ->
        IO.puts("Customer got a haircut and leaves.")
    end
  end

  def update_barber(new_function) do
    send(self(), {:update_barber_behavior, new_function})
  end

  def take_barber_break do
    send(self(), {:take_break})
  end

  def add_barber_chair do
    send(self(), {:add_chair})
  end

  defp loop_forever do
    receive do
      _ -> loop_forever()
    end
  end
end

BarberShop.start()
