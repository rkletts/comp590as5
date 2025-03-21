defmodule BarberShop do
  def start(chairs) do
    shop = self()
    barber = spawn_link(fn -> barber_loop(shop, chairs) end)
    waiting_room = spawn_link(fn -> waiting_room_loop([], chairs, barber) end)
    receptionist = spawn_link(fn -> receptionist_loop(waiting_room, barber) end)
    
    # Ensure barber starts in sleeping state
    send(barber, :barber_idle)
    
    spawn_link(fn -> customer_generator(receptionist, 1) end)  # Start generating customers
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
            IO.puts("Barber wakes up to cut hair for customer #{customer.id}...")
            behavior.(customer)
            send(shop, :barber_ready)
            barber_loop(shop, chairs, behavior)
        after
          5000 ->  # Optional timeout to keep barber asleep if no customers arrive
            IO.puts("Barber remains asleep, waiting for customers...")
            barber_loop(shop, chairs, behavior)
        end
    end
  end

  defp default_haircut(customer) do
    IO.puts("Barber is cutting hair for customer #{customer.id}...")
    :timer.sleep(:rand.uniform(3000))  # Simulate haircut time
    IO.puts("Barber finished haircut for customer #{customer.id}.")
    send(customer.pid, :haircut_done)
  end

  defp waiting_room_loop(queue, chairs, barber) do
    receive do
      {:enter, customer, receptionist} ->
        if length(queue) < chairs do
          IO.puts("Customer #{customer.id} takes a seat in the waiting room.")
          send(receptionist, :seated)
          waiting_room_loop(queue ++ [customer], chairs, barber)
        else
          IO.puts("Waiting room full. Customer #{customer.id} leaves.")
          send(receptionist, :full)
          waiting_room_loop(queue, chairs, barber)
        end
      
      {:next_customer, barber} when queue != [] ->
        [next | rest] = queue
        send(barber, {:cut_hair, next})
        waiting_room_loop(rest, chairs, barber)
      
      {:next_customer, _barber} ->  # No customers to serve, barber goes idle
        send(barber, :barber_idle)
        waiting_room_loop(queue, chairs, barber)
    end
  end

  defp receptionist_loop(waiting_room, barber) do
    receive do
      {:new_customer, customer} ->
        IO.puts("Receptionist greets customer #{customer.id}.")
        send(waiting_room, {:enter, customer, self()})
        send(waiting_room, {:next_customer, barber})
      
      :barber_ready ->
        send(waiting_room, {:next_customer, barber})
      
      :no_customers ->  # When no customers are left, barber goes idle
        send(barber, :barber_idle)
    end
    receptionist_loop(waiting_room, barber)
  end

  defp customer_generator(receptionist, customer_id) do
    :timer.sleep(:rand.uniform(2000))  # Random time for next customer to arrive
    customer = spawn(fn -> customer_loop(customer_id) end)
    send(receptionist, {:new_customer, %{id: customer_id, pid: customer}})
    customer_generator(receptionist, customer_id + 1)  # Increment customer ID for the next customer
  end

  defp customer_loop(customer_id) do
    receive do
      :haircut_done ->
        IO.puts("Customer #{customer_id} leaves after haircut.")
    end
  end

  defp loop_forever do
    receive do
      _ -> loop_forever()
    end
  end
end

# Start the simulation with 6 waiting room chairs (as specified)
BarberShop.start(6)
