(ns runner_puncher.ssw
  (:import [javax.swing JFrame JLabel AbstractAction Timer]
           [java.awt Canvas Dimension]
           [java.awt.event KeyAdapter MouseListener MouseMotionListener]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn new-super-simple-window
  "Create and return a Canvas within a JFrame using a bunch of optional settings and callbacks.
  A Timer is setup if :fps or :on-timer is given.
  Options:
    :title - Exactly what it sounds like. Defaults to \"Super simple window\".
    :width - The width of the Canvas within the window. Defaults to 640.
    :height - The height of the Canvas within the window. Defaults to 480.
    :fps - How often the timer (if used) runs. Defaults to 30.
    :on-timer - Callback that receives a ... object.
    :on-render - Callback that receives a Graphics object.
    :on-key-press - Callback that receives a KeyEvent object.
    :on-mouse-move - Cllback that receives a MouseEvent object
    :on-mouse-press - Callback that receives a KeyEvent object."
  [options]
  (let [#^JFrame frame (JFrame. #^String (:title options "Super simple window"))
        #^AbstractAction action (proxy [AbstractAction] []
                                  (actionPerformed [e]
                                    ((:on-timer options identity) e)
                                    (.repaint frame)))
        #^Timer timer (Timer. (/ 1000.0 (:fps options 30)) action)
        #^Canvas canvas (proxy [JLabel] []
                          (paintComponent  [g]
                            ((:on-render options identity) g)))]
    (doto canvas
      (.setIgnoreRepaint true)
      (.setFocusable true)
      (.setPreferredSize (Dimension. (:width options 640) (:height options 480)))
      (.addKeyListener (proxy [KeyAdapter] []
                         (keyPressed [e]
                           ((:on-key-press options identity) e)
                           (.repaint frame))))
      (.addMouseMotionListener (proxy [MouseMotionListener] []
                                 (mouseMoved [e]
                                             ((:on-mouse-move options identity) e)
                                             (.repaint frame))
                                 (mouseDragged [e]
                                             ((:on-mouse-move options identity) e)
                                             (.repaint frame))))
      (.addMouseListener (proxy [MouseListener] []
                           (mousePressed [e]
                                         ((:on-mouse-press options identity) e)
                                         (.repaint frame))
                           (mouseClicked [e]
                                         ((:on-mouse-press options identity) e)
                                         (.repaint frame))
                           (mouseEntered [e])
                           (mouseExited [e])
                           (mouseReleased [e]))))
    (doto frame
      (.add canvas)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.pack)
      (.setVisible true))
    (when (:fps options)
      (.start timer))
    canvas))

