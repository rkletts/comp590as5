defmodule BarberShop do
  def start(chairs) do
    shop = self()
    barber = spawn_link(fn -> barber_loop(shop, chairs) end)
    waiting_room = spawn_link(fn -> waiting_room_loop([], chairs, barber) end)
    receptionist = spawn_link(fn -> receptionist_loop(waiting_room, barber) end)
    spawn_link(fn -> customer_generator(receptionist) end)
    loop_forever()
  end

  defp barber_loop(shop, chairs, behavior \\ &default_haircut/1) do
    receive do
      {:cut_hair, customer} ->
        behavior.(customer)
        send(shop, :barber_ready)
        barber_loop(shop, chairs, behavior)
      
      {:update_barber_behavior, new_function} ->
        IO.puts("Updating barber behavior...")
        barber_loop(shop, chairs, new_function)
      
      :barber_idle ->
        IO.puts("Barber is sleeping, waiting for customers...")
        receive do
          {:cut_hair, customer} ->
            IO.puts("Barber wakes up to cut hair...")
            behavior.(customer)
            send(shop, :barber_ready)
            barber_loop(shop, chairs, behavior)
        end
    end
  end

  defp default_haircut(customer) do
    IO.puts("Barber is cutting hair...")
    :timer.sleep(:rand.uniform(3000))
    IO.puts("Barber finished haircut.")
    send(customer, :haircut_done)
  end

  defp waiting_room_loop(queue, chairs, barber) do
    receive do
      {:enter, customer, receptionist} ->
        if length(queue) < chairs do
          IO.puts("Customer takes a seat in the waiting room.")
          send(receptionist, :seated)
          waiting_room_loop(queue ++ [customer], chairs, barber)
        else
          IO.puts("Waiting room full. Customer leaves.")
          send(receptionist, :full)
          waiting_room_loop(queue, chairs, barber)
        end
      
      {:next_customer, barber} when queue != [] ->
        [next | rest] = queue
        send(barber, {:cut_hair, next})
        waiting_room_loop(rest, chairs, barber)
      
      {:next_customer, _barber} ->
        send(barber, :barber_idle)
        waiting_room_loop(queue, chairs, barber)
    end
  end

  defp receptionist_loop(waiting_room, barber) do
    receive do
      {:new_customer, customer} ->
        IO.puts("Receptionist greets a new customer.")
        send(waiting_room, {:enter, customer, self()})
        send(waiting_room, {:next_customer, barber})
      
      :barber_ready ->
        send(waiting_room, {:next_customer, barber})
      
      :no_customers ->
        send(barber, :barber_idle)
    end
    receptionist_loop(waiting_room, barber)
  end

  defp customer_generator(receptionist) do
    :timer.sleep(:rand.uniform(2000))
    customer = spawn(fn -> customer_loop() end)
    send(receptionist, {:new_customer, customer})
    customer_generator(receptionist)
  end

  defp customer_loop do
    receive do
      :haircut_done ->
        IO.puts("Customer leaves after haircut.")
    end
  end

  defp loop_forever do
    receive do
      _ -> loop_forever()
    end
  end
end

# Start the simulation with 3 waiting room chairs
BarberShop.start(3)
