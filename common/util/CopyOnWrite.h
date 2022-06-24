#include <utility>

#include "common/util/Assert.h"

/*
template<typename T, typename... Args>
std::unique_ptr<T> make_unique(Args&&... args)
{
    return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
}
 */

/*!
 * The CopyOnWrite class acts like a value, but internally uses references to avoid copying
 * when it is possible to avoid it.
 *
 * It is used like a shared pointer.
 * But, if you try to modify an existing object with multiple owners, it will make a copy
 * so the other owners don't see any changes.  In this way, it does not act like a reference.
 *
 * To construct a new object, use CopyOnWrite<T>(args...). This is different from the usual smart
 * pointer pattern.
 *
 * Like shared pointers, a CopyOnWrite can be null.  Doing mut() just gives you a null pointer.
 *
 * The default .get(), ->, and * operators give you const references.  If you need to modify,
 * use .mut().  It will create a copy if needed, then give you a mutable reference.
 */
template <typename T>
class CopyOnWrite {
 private:
  // we store the object and its reference count in the same heap allocation.
  struct ObjectAndCount {
    T object;

    // construct the object in-place, or copy construct from an existing.
    template <typename... Args>
    explicit ObjectAndCount(Args&&... args) : object(std::forward<Args>(args)...) {}
    explicit ObjectAndCount(const T& existing) : object(existing) {}

    // in case we ever want this to have locks.
    void add_ref() { m_count++; }
    void remove_ref() { m_count--; }
    bool unique() { return m_count == 1; }
    bool dead() { return m_count == 0; }

   private:
    int m_count = 0;
  };

 public:
  CopyOnWrite() = default;  // allow nulls.

  /*!
   * Construct a new object.
   */
  template <typename... Args>
  explicit CopyOnWrite(Args&&... args) {
    auto obj = new ObjectAndCount(std::forward<Args>(args)...);
    acquire_object(obj);
  }

  /*!
   * Copy an object.
   */
  CopyOnWrite(const CopyOnWrite<T>& other) { acquire_object(other.m_data); }

  CopyOnWrite<T>& operator=(const CopyOnWrite<T>& other) {
    if (this == &other) {
      return *this;
    }

    if (m_data != other.m_data) {
      clear_my_object();
      acquire_object(other.m_data);
    }
    return *this;
  }

  ~CopyOnWrite() { clear_my_object(); }

  // constant access
  const T* get() const { return &m_data->object; }
  const T* operator->() const { return &m_data->object; }
  const T& operator*() const { return m_data->object; }
  explicit operator bool() const { return m_data; }

  T* mut() {
    if (!m_data) {
      return nullptr;
    }

    if (!m_data->unique()) {
      ASSERT(!m_data->dead());
      m_data->remove_ref();  // don't need to check for dead here, there's another ref somewhere.
      ASSERT(!m_data->dead());
      m_data = new ObjectAndCount(m_data->object);
      m_data->add_ref();
    }
    return &m_data->object;
  }

 private:
  void clear_my_object() {
    if (m_data) {
      m_data->remove_ref();
      if (m_data->dead()) {
        delete m_data;
      }
    }
    m_data = nullptr;
  }

  void acquire_object(ObjectAndCount* obj) {
    ASSERT(!m_data);
    m_data = obj;
    if (obj) {
      m_data->add_ref();
    }
  }

  ObjectAndCount* m_data = nullptr;
};
