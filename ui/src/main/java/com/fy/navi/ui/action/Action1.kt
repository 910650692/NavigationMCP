package com.fy.navi.ui.action

/**
 * @Description 注册点击事件,携带返回值{View.getTag}
 * @Author lvww
 * @date 2024/11/22
 */
interface Action1<T> {
    fun call(t: T)
}