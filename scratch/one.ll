; ModuleID = 'one.bc'

declare i32 @putchar(i32)

define void @main() {
  %1 = call i32 @add(i32 42, i32 1)
  call i32 @putchar(i32 %1)
  ret void
}

define i32 @add(i32 %a, i32 %b) {
  %1 = add i32 %a, %b
  ret i32 %1
}

