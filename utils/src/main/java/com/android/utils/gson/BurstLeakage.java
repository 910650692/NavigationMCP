package com.android.utils.gson;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/21
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({java.lang.annotation.ElementType.FIELD})
public @interface BurstLeakage {
    boolean serialize() default true;//默认序列化生效

    boolean deserialize() default true;//默认反序列化生效
}
