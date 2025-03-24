package com.fy.navi.scene.ui.favorite;


import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.view.Gravity;

import com.fy.navi.ui.view.SkinRadioButton;

public class DrawableCenterRadioButton extends SkinRadioButton {

    public DrawableCenterRadioButton(Context context) {
        super(context);
    }

    public DrawableCenterRadioButton(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public DrawableCenterRadioButton(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected void onDraw(Canvas canvas) {
        Drawable buttonDrawable = getButtonDrawable();
        super.onDraw(canvas);
        if (buttonDrawable != null) {
            // 根据选中状态选择不同的drawable
            buttonDrawable.setState(getDrawableState());
            // 居中方式
            final int verticalGravity = getGravity() & Gravity.VERTICAL_GRAVITY_MASK;
            // Drawable宽度
            final int drawableWidth = buttonDrawable.getIntrinsicWidth();
            // Drawable高度
            final int drawableHeight = buttonDrawable.getIntrinsicHeight();
            // 顶部位置
            int top = 0;
            switch (verticalGravity) {
                case Gravity.BOTTOM:
                    top = getHeight() - drawableHeight;
                    break;
                case Gravity.CENTER_VERTICAL:
                    top = (getHeight() - drawableHeight) / 2;
                    break;
            }
            // 底部位置
            int bottom = top + drawableHeight;
            // 左边位置（与父类不同）
            int left = (getWidth() - drawableWidth) / 2;
            // 右边位置（与父类不同）
            int right = left + drawableWidth;
            // 绘制边界
            buttonDrawable.setBounds(left, top, right, bottom);
            // 绘制控件
            buttonDrawable.draw(canvas);
            super.onDraw(canvas);
        }
    }
}