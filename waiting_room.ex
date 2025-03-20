defmodule WaitingRoom do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(state) do
    {:ok, state}
  end

  def add_customer(customer) do
    GenServer.call(__MODULE__, {:add_customer, customer})
  end

  def handle_call({:add_customer, customer}, _from, state) do
    if length(state) < 6 do
      IO.puts("Customer added to waiting room")
      {:reply, :ok, [customer | state]}
    else
      IO.puts("Waiting room full, customer turned away.")
      {:reply, :full, state}
    end
  end
end
