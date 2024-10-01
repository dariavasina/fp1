def sum_of_divisors(n):
    return sum(i for i in range(1, n // 2 + 1) if n % i == 0)

def amicable_numbers(limit):
    s = 0
    for a in range(1, limit):
        b = sum_of_divisors(a)
        if b != a and sum_of_divisors(b) == a:
            s += a

    return s

result = amicable_numbers(10000)
print("Sum of all amicable numbers under 10,000:", result)
