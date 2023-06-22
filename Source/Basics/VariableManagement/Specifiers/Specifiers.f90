module Specifiers

  use KIND_Singleton, &
        KI  => KIND_INTEGER, &
        KIB => KIND_INTEGER_BIG, &
        KR  => KIND_REAL, &
        KC  => KIND_COMPLEX, &
        KL  => KIND_LOGICAL
  use LEN_Singleton, &
        LN => LEN_NUMBER, &
        LL => LEN_LABEL, &
        LF => LEN_FILENAME, &
        LB => LEN_BUFFER
  use Void_Base
  use LabeledValue_Base

end module Specifiers
