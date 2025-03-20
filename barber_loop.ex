defmodule BarberLoop do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    send(self(), :cut_hair)
    {:ok, :idle}
  end

  def handle_info(:cut_hair, state) do
    IO.puts("Barber starts cutting hair.")
    :timer.sleep(:rand.uniform(2000)) 
    send(self(), :cut_hair)
    {:noreply, state}
  end

  def change_behavior(new_cut_time) do
    IO.puts("Barber's behavior changed!")
  end
end
