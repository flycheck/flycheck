function foo(dist, p) result(value)
    integer :: value
    integer, intent(in) :: dist
    integer, intent(in) :: p

    value = dist+value
end function foo

program hello
interface
   subroutine bar(a,b)
     integer, intent(in) :: a
     integer, intent(out) :: b
   end subroutine bar
end interface
integer :: a

call bar(a,a)

end program hello
