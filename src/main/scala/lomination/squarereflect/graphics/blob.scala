// package lomination.squarereflect.graphics

// import java.nio.IntBuffer

// import org.lwjgl.opengl.GL
// import org.lwjgl.opengl.GL11
// import org.lwjgl.system.MemoryStack
// import org.lwjgl.glfw.Callbacks
// import org.lwjgl.glfw.GLFW
// import org.lwjgl.glfw.GLFWErrorCallback
// import org.lwjgl.glfw.GLFWVidMode

// import org.lwjgl.system.MemoryStack.*
// import org.lwjgl.system.MemoryUtil.*


// class Boot:
//   private var window: Long
//   def init: Unit =
//     GLFWErrorCallback.createPrint(System.err).set()
		
//     if(!GLFW.glfwInit()) throw new IllegalStateException("Unable to initialize GLFW")
        
//     GLFW.glfwDefaultWindowHints()
//     GLFW.glfwWindowHint(GLFW.GLFW_VISIBLE, GLFW.GLFW_FALSE)
//     GLFW.glfwWindowHint(GLFW.GLFW_RESIZABLE, GLFW.GLFW_TRUE)

//     window = GLFW.glfwCreateWindow(640, 480, "LWJGL Bootcamp", NULL, NULL)
    
//     if(window == NULL) throw new IllegalStateException("Unable to create GLFW Window")
        
//     GLFW.glfwSetKeyCallback(window, (window, key, scancode, action, mods) -> {})
        
//     try(val stack: MemoryStack = stackPush()){
//         val pWidth: IntBuffer = stack.mallocInt(1)
//         val pHeight: IntBuffer = stack.mallocInt(1)
        
//         GLFW.glfwGetWindowSize(window, pWidth, pHeight)
          
//         val vidmode: GLFWVidMode = GLFW.glfwGetVideoMode(GLFW.glfwGetPrimaryMonitor())
        
//         GLFW.glfwSetWindowPos(window,(vidmode.width() - pWidth.get(0)) / 2,(vidmode.height() - pHeight.get(0)) / 2)
          
//         GLFW.glfwMakeContextCurrent(window)
//         GLFW.glfwSwapInterval(1)
//         GLFW.glfwShowWindow(window)
//     }

//   def run: Unit
//   def loop: Unit