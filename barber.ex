defmodule Barber do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    {:ok, :idle}
  end

  def handle_info(:cut_hair, state) do
    cut_time = :rand.uniform(1000)
    IO.puts("Barber is cutting hair for #{cut_time} ms.")
    :timer.sleep(cut_time)
    send(self(), :cut_hair)
    {:noreply, state}
  end
end
