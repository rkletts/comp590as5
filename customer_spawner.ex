defmodule CustomerSpawner do
  def spawn_customer(wait_time) do
    Customer.start_link(wait_time)
  end
end
