/*!
 * IRegister is the Register for the Intermediate Representation.
 */

#ifndef JAK_IREGISTER_H
#define JAK_IREGISTER_H

#include <string>

class IRegister {
 public:
  enum Kind {
    GPR,
    XMM_FLOAT,
    XMM_INT,
    XMM_VF
  };

  int id = -1;

  std::string to_string(bool with_constraints = false);
};

#endif  // JAK_IREGISTER_H
