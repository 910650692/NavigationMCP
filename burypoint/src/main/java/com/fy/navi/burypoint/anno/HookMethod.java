package com.fy.navi.burypoint.anno;


import com.flyjingfish.android_aop_annotation.anno.AndroidAopPointCut;
import com.fy.navi.burypoint.cut.HookPointCut;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@AndroidAopPointCut(HookPointCut.class)
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface HookMethod {

    String eventName() default "";
    String sid() default "S00000008";
    String svid() default "SV00000088";

}
