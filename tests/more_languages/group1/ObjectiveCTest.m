// ObjectiveCTest.m
#import <Foundation/Foundation.h>

@interface HelloWorld: NSObject
- (void) sayHello;
@end

@implementation HelloWorld
- (void) sayHello {
    NSLog(@"Hello, World!");
}

void sayHelloWorld() {
    NSLog(@"Hello, World!");
}
@end
