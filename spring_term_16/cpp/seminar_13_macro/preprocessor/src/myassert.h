#ifndef __MY_ASSERT_H__
#define __MY_ASSERT_H__

#ifndef CONFIG_DEBUG
#define CONFIG_DEBUG 0
#endif


// TODO
#if (CONFIG_DEBUG == 0)
#    define myassert(cond) \
         (void)(0)
#else
#include <iostream>
#    define myassert(cond)                                                           \
         do {                                                                        \
	         if (!(cond)) {                                                          \
	         	std::cerr << "Assertion [ " << #cond << " ] failed!" << std::endl;   \
	         	std::cerr << "     File:     [" << __FILE__ << "]" << std::endl;     \
	         	std::cerr << "     Function: [" << __func__ << "]" << std::endl;     \
	         	std::cerr << "     Line:     [" << __LINE__ << "]" << std::endl;     \
	         	abort();                                                             \
	         }                                                                       \
         } while(0)                    
#endif // CONFIG_DEBUG == 0


#endif //__MY_ASSERT_H__
