package diku.oopd.util;

import org.eclipse.jdt.annotation.Nullable;

public abstract class ResourceBundle<K,V>
{
  private HashMap<K,V> resourceBundle;
  
  protected ResourceBundle()
  {
    this.resourceBundle = new HashMap<>();
  }
  
  @Nullable
  public V getValue(K key)
  {
    return this.resourceBundle.get(key);
  }
}
