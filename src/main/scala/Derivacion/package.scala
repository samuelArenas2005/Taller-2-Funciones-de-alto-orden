package object Derivacion {
  val h = 0.1;
  def derivada(f:Double => Double): Double => Double = {
    def derivadaFunction(x: Double):Double = {
      (f(x-2*h)-8*f(x-h)+8*f(x+h)-f(x+2*h))/(12*h)
    }
    derivadaFunction
  }

  def derivadaSuma(f: Double => Double, g: Double => Double): Double => Double = {
    def derivadaSumaFunction(x: Double): Double = {
      derivada(f)(x) + derivada(g)(x)
    }

    derivadaSumaFunction
  }

  def derivadaResta(f: Double => Double, g: Double => Double): Double => Double = {
    def derivadaRestaFunction(x: Double): Double = {
      derivada(f)(x) - derivada(g)(x)
    }
    derivadaRestaFunction
  }

  def derivadaMult(f: Double => Double, g: Double => Double): Double => Double = {
    def derivadaMultFunction(x: Double): Double = {
      derivada(f)(x)*g(x) + f(x)*derivada(g)(x)
    }

    derivadaMultFunction
  }

  def derivadaDiv(f: Double => Double, g: Double => Double): Double => Double = {
    def derivadaDivFunction(x: Double): Double = {
      (derivada(f)(x) * g(x) - f(x) * derivada(g)(x))/(g(x)*g(x))
    }

    derivadaDivFunction
  }



}
