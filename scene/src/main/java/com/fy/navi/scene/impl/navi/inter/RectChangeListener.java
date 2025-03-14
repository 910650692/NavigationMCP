package com.fy.navi.scene.impl.navi.inter;


import android.view.View;

import com.fy.navi.scene.impl.navi.common.AutoUIViewRect;


public interface RectChangeListener {

    /**
     * 位置大小发生变化时回调矩形区域变化
     *
     * @param newRect 新的位置大小
     * @param oldRect 旧的位置大小
     */
    void onRectChange(View view, AutoUIViewRect newRect, AutoUIViewRect oldRect);
}
