package com.sgm.navi.ui.action

/**
 * @Description 注册点击事件,携带返回值,自定义属性param{View.getParam}
 * @Author lvww
 * @date 2024/11/22
 */
interface Action2<P> {
    fun call(t: P)
}