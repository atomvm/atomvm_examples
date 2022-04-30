defmodule LispFacesTest do
  use ExUnit.Case
  doctest LispFaces

  test "greets the world" do
    assert LispFaces.hello() == :world
  end
end
