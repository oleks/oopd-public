package diku.oopd.util;

import org.eclipse.jdt.annotation.Nullable;

public abstract class ResourceBundle<K,V>
{
  private HashMap<K,V> resourceBundle;
  
  protected ResourceBundle()
  {
    this.resourceBundle = new HashMap<>();
  }
  
  public V getValue(K key)
  {
    V result = this.resourceBundle.get(key);
    
    if (result == null)
    {
      return (V)(new Object());
    }
    return result;
  }
}
