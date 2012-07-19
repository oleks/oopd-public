package diku.oopd.regneark;

import diku.oopd.regneark.view.MainView;

public class Main
{
  public static void main(String[] _)
  {
    java.awt.EventQueue.invokeLater(new Runnable()
    {
      @Override
      public void run()
      {
        new MainView().setVisible(true);
      }
    });
  }
}
