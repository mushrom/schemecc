(import (scheme base))

(primitive-call + 2 (primitive-call - (primitive-call + 40 40) 40))
