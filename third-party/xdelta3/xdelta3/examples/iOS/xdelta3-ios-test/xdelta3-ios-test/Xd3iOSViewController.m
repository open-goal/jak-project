/* xdelta3 - delta compression tools and library -*- Mode: objc *-*
   Copyright 2016 Joshua MacDonald

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#import "Xd3iOSViewController.h"
#include "xdelta3.h"
#include "dispatch/queue.h"
#include "Foundation/NSBundle.h"

extern void (*xprintf_message_func)(const char* msg);
void print_to_view(const char* buf);
int xd3_main_cmdline(int argc, char **argv);
void do_localfile_test(void);
int compare_files(const char* file1, const char* file2);
Xd3iOSViewController *static_ptr;

@implementation Xd3iOSViewController
@synthesize theSeed = _theSeed;
@synthesize theView = _theView;
@synthesize theOutput = _theOutput;
@synthesize inTest = _inTest;

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
}

#pragma mark - View lifecycle

- (void)viewDidLoad
{
    [super viewDidLoad];
}

- (void)viewDidUnload
{
    [self setTheSeed:nil];
    [self setTheView:nil];
    [self setTheView:nil];
    [super viewDidUnload];
}

- (void)viewWillAppear:(BOOL)animated
{
    [super viewWillAppear:animated];
}

- (void)viewDidAppear:(BOOL)animated
{
    [super viewDidAppear:animated];
}

- (void)viewWillDisappear:(BOOL)animated
{
	[super viewWillDisappear:animated];
}

- (void)viewDidDisappear:(BOOL)animated
{
	[super viewDidDisappear:animated];
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    switch (interfaceOrientation) {
        case UIInterfaceOrientationPortrait:
        case UIInterfaceOrientationPortraitUpsideDown:
            return YES;
        default:
            break;
    }
    return NO;
}
- (BOOL)textFieldShouldReturn:(UITextField*)theTextField {
    if (theTextField == self.theSeed) {
        [theTextField resignFirstResponder];
    }
    return YES;
}
- (IBAction)startTest:(id)sender {
    if (self.inTest) {
        return;
    }
    self.inTest = YES;
    NSString *seedString = self.theSeed.text;
    if ([seedString length] == 0) {
        seedString = @"RFC3284";
    }
    static_ptr = self;
    xprintf_message_func = &print_to_view;
    self.theOutput = [[NSMutableString alloc] initWithFormat:@"Starting test (seed=%@)\n", seedString];
    self.theView.text = self.theOutput;
    dispatch_queue_t mq = dispatch_get_main_queue();
    dispatch_queue_t dq = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);
    dispatch_async(dq, ^{
        do_localfile_test();
        char *argv[] = { "xdelta3", "test", NULL };
        xd3_main_cmdline(2, argv);
        print_to_view("Finished unittest: success");
        dispatch_async(mq, ^{
            self.inTest = NO;
        });
    });
}

void printns_to_view(NSString* ns);
void printns_to_view(NSString* ns) {
    dispatch_queue_t mq = dispatch_get_main_queue();
    dispatch_async(mq, ^{
        if ([static_ptr.theOutput length] < 25000) {
            [static_ptr.theOutput appendString:ns];
        } else {
            static_ptr.theOutput = [[NSMutableString alloc] initWithString:ns];
        }
        static_ptr.theView.text = static_ptr.theOutput;
        CGSize size = static_ptr.theView.contentSize;
        [static_ptr.theView scrollRectToVisible:CGRectMake(0, size.height - 1, 1, 1) animated:NO];
    });
}

void print_to_view(const char* buf) {
    NSString *ns = [NSString stringWithCString:buf encoding:NSASCIIStringEncoding];
    printns_to_view(ns);
}

void do_localfile_test(void) {
    NSBundle *bundle;
    bundle = [NSBundle mainBundle];
    NSString *localfile1 = [bundle pathForResource:@"file_v1" ofType:@"bin"];
    NSString *localfile2 = [bundle pathForResource:@"file_v2" ofType:@"bin"];
    NSString *localfiled = [bundle pathForResource:@"file_v1_to_v2" ofType:@"bin"];
    printns_to_view([localfile1 stringByAppendingString:@"\n"]);
    printns_to_view([localfile2 stringByAppendingString:@"\n"]);
    printns_to_view([localfiled stringByAppendingString:@"\n"]);
    NSString *tmpdir = NSTemporaryDirectory();
    NSString *tmpfile = [tmpdir stringByAppendingPathComponent:@"delta.tmp"];
    printns_to_view([tmpfile stringByAppendingString:@"\n"]);
    char *argv[] = { 
        "xdelta3", "-dfvv", "-s", 
        (char*)[localfile1 UTF8String],
        (char*)[localfiled UTF8String],
        (char*)[tmpfile UTF8String] };
    xd3_main_cmdline(6, argv);

    NSFileManager *filemgr;

    filemgr = [NSFileManager defaultManager];
    
    if ([filemgr contentsEqualAtPath: localfile2 andPath: tmpfile] == YES) {
        printns_to_view(@"File contents match\n");
    } else {
        NSError *err1 = NULL;
        NSDictionary *d1 = [filemgr attributesOfItemAtPath: tmpfile error: &err1];
        if (err1 != NULL) {
            printns_to_view([@"File localfile2 could not stat %s\n" stringByAppendingString: tmpfile]);
        } else {
            printns_to_view([@"File contents do not match!!!! tmpfile size=" stringByAppendingString:
                             [[NSMutableString alloc] initWithFormat:@"%llu\n", [d1 fileSize]]]);
        }
        compare_files([localfile2 UTF8String], [tmpfile UTF8String]);
    }
    print_to_view("Finished localfile test.\n");
}

@end
