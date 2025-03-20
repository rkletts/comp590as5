defmodule Customer do
  def start_link(wait_time) do
    Task.start_link(fn -> loop(wait_time) end)
  end

  defp loop(wait_time) do
    :timer.sleep(wait_time)
    IO.puts("Customer arrives!")
    loop(wait_time)
  end
end
