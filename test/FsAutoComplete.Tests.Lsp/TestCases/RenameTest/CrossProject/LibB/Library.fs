namespace LibB

open LibA.Say

module Say =
    let hi name =
        String.replicate 2 (hello name)

    let twice name =
        String.replicate 2 (LibA.Say.yell name)
