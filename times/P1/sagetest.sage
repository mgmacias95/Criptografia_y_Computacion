primelist = [46381,768479,9476407,36780481,562390847,1894083629,65398261921,364879542899,8590356927553,28564333765949,123456789101119]
explist = [1749924,1749924,1749925,1749925,1749926,1749924,1749925,1749925]
factorizar = [46382,768481,9476408,36780481,562390848,1894083630,65398261923,36487954297,8590365927554,28564333765948,123456789101121]

print("Ejecutando test de Miller Rabin")
for prime in primelist:
    antes = walltime()
    is_prime(prime)
    despues = walltime()
    print("Tiempo " + str(prime) + ": " + str(despues-antes))

print("Ejecutando test de Baby Step Giant Step")
for (prime,e) in zip(primelist[:len(explist)],explist):
    antes = walltime()
    bsgs(Mod(123456,prime), Mod(e,prime), (0,prime-1))
    despues = walltime()
    print("Tiempo " + str(prime) + ": " + str(despues - antes))

print("Ejecutando factorización")
for n in factorizar:
    antes = walltime()
    factor(n)
    despues = walltime()
    print("Tiempo " + str(n) + ": " + str(despues-antes))
