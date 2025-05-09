package com.fy.navi.scene.ui.setting;


import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Canvas;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;

import com.fy.navi.scene.R;
import com.fy.navi.ui.view.SkinRadioButton;

public class DrawableCenterRadioButton extends SkinRadioButton {

    private float mDrawableWidth = 80; // 默认80dp
    private float mDrawableHeight = 80; // 默认80dp
    private int mTextColor; // 颜色字段
    private int mSelectedColor; // 新增选中颜色
    private int mNormalColor;  // 新增默认颜色

    public DrawableCenterRadioButton(final Context context, final AttributeSet attrs) {
        super(context, attrs);
        initAttributes(context, attrs);
    }

    public DrawableCenterRadioButton(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        initAttributes(context, attrs);
    }


    /**
     * 初始化属性
     * @param context 上下文
     * @param attrs 属性
     */
    private void initAttributes(final Context context, final AttributeSet attrs) {
        final TypedArray ta = context.obtainStyledAttributes(attrs, R.styleable.DrawableCenterRadioButton);
        mDrawableWidth = ta.getFloat(R.styleable.DrawableCenterRadioButton_drawableWidth, 80);
        mDrawableHeight = ta.getFloat(R.styleable.DrawableCenterRadioButton_drawableHeight, 80);
        mNormalColor = ta.getColor(R.styleable.DrawableCenterRadioButton_normalTextColor, getCurrentTextColor());
        mSelectedColor = ta.getColor(R.styleable.DrawableCenterRadioButton_selectedTextColor, getCurrentTextColor());
        mTextColor = isChecked() ? mSelectedColor : mNormalColor; // 初始化当前颜色
        ta.recycle();

        // 添加选中监听
        setOnCheckedChangeListener((buttonView, isChecked) -> {
            mTextColor = isChecked ? mSelectedColor : mNormalColor;
            invalidate();
        });
    }

    @Override
    protected void onDraw(final Canvas canvas) {
        final Drawable[] drawables = getCompoundDrawables();
        final Drawable drawableStart = drawables[0];

        if (drawableStart != null) {
            // 获取实际设置的padding值
            final int paddingStart = getPaddingStart();
            final int drawableWidth = (int)mDrawableWidth;
            final int drawableHeight = (int)mDrawableHeight;

            // 设置drawable边界
            drawableStart.setBounds(0, 0, drawableWidth, drawableHeight);

            // 计算垂直居中位置
            final int drawableTop = (getHeight() - drawableHeight) / 2;

            // 绘制drawable
            canvas.save();
            canvas.translate(paddingStart, drawableTop);
            drawableStart.draw(canvas);
            canvas.restore();

            // 计算文字绘制位置（垂直居中）
            final int textX = paddingStart + drawableWidth + getCompoundDrawablePadding();
            final float textY = (getHeight() - (getPaint().descent() + getPaint().ascent())) / 2;

            // 绘制文字
            getPaint().setColor(mTextColor); // 设置字体颜色
            canvas.drawText(getText().toString(), textX, textY, getPaint());
        } else {
            super.onDraw(canvas);
        }
    }

}