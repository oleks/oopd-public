package diku.oopd.util;

import org.eclipse.jdt.annotation.Nullable;

public class HashMap<K,V>
{
  private java.util.HashMap<K,V> hashMap;
  
  public HashMap()
  {
    this.hashMap = new java.util.HashMap<>();
  }
  
  @Nullable
  public V get(K key)
  {
    return this.hashMap.get(key);
  }
  
  public void put(K key, V value)
  {
    this.hashMap.put(key, value);
  }
}
