defmodule BarberShop do
  def start do
    spawn(__MODULE__, :receptionist_loop, [[]])
  end

  def receptionist_loop(waiting_room) do
    receive do
      {:new_customer, customer_pid} ->
        if length(waiting_room) < 6 do
          IO.puts("Receptionist greets a new customer.")
          send(customer_pid, :sit)
          receptionist_loop(waiting_room ++ [customer_pid])
        else
          IO.puts("Receptionist turns away a customer; waiting room is full.")
          send(customer_pid, :leave)
          receptionist_loop(waiting_room)
        end

      {:barber_ready, barber_pid} ->
        case waiting_room do
          [next_customer | rest] ->
            send(barber_pid, {:next_customer, next_customer})
            receptionist_loop(rest)
          [] ->
            send(barber_pid, :no_customers)
            receptionist_loop([])
        end
    end
  end

  def customer_generator do
    spawn(fn -> loop_customer_generator() end)
  end

  defp loop_customer_generator do
    Process.sleep(:rand.uniform(2000))
    customer_pid = spawn(__MODULE__, :customer_loop, [])
    send(Process.whereis(:receptionist), {:new_customer, customer_pid})
    loop_customer_generator()
  end

  def customer_loop do
    receive do
      :sit ->
        IO.puts("Customer takes a seat in the waiting room.")
        receive do
          :get_haircut ->
            IO.puts("Customer is getting a haircut.")
            receive do
              :done ->
                IO.puts("Customer leaves after haircut.")
            end
        end
      :leave ->
        IO.puts("Customer leaves because the shop is full.")
    end
  end

  def barber do
    spawn(fn -> barber_loop() end)
  end

  defp barber_loop do
    send(Process.whereis(:receptionist), {:barber_ready, self()})
    receive do
      {:next_customer, customer_pid} ->
        IO.puts("Barber is cutting hair...")
        Process.sleep(:rand.uniform(3000))
        IO.puts("Barber finished haircut.")
        send(customer_pid, :done)
        barber_loop()
      :no_customers ->
        IO.puts("Barber is taking a short break.")
        Process.sleep(:rand.uniform(5000))
        barber_loop()
    end
  end
end

# Start the simulation
Process.register(BarberShop.start(), :receptionist)
BarberShop.barber()
BarberShop.customer_generator()
