defmodule Receptionist do
  def greet_and_direct(customer) do
    case WaitingRoom.add_customer(customer) do
      :ok -> IO.puts("Receptionist: Customer added to the waiting room.")
      :full -> IO.puts("Receptionist: No space, customer turned away.")
    end
  end
end
