primelist = [46381,768479,9476407,36780481,562390847,1894083629,65398261921,364879542899,8590356927553,28564333765949,123456789101119]

print("Ejecutando test de Miller Rabin")
for prime in primelist:
    antes = walltime()
    is_prime(prime)
    despues = walltime()
    print("Tiempo " + str(prime) + ": " + str(despues-antes))

print("Ejecutando test de Baby Step Giant Step")
for prime in primelist:
    antes = walltime()
    sage.groups.generic.bsgs(123456, 1749924, prime)
    despues = walltime()
    print("Tiempo " + str(prime) + ": " + str(despues - antes))
