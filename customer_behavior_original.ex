defmodule CustomerBehavior do
  def wait_for_haircut(barber, id) do
    receive do
      :haircut_done ->
        IO.puts("Customer #{id} got haircut and leaves happy")
        exit(:normal)
    end
  end
end
