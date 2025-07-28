package com.sgm.navi.ui.view;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintLayout;

import com.sgm.navi.ui.R;

public class SkinConstraintLayout extends ConstraintLayout {

    /**
     * true：点击设置60%不透明度，抬起恢复原状（按压态）
     */
    private boolean mIsClickChangeColor = false;

    public SkinConstraintLayout(final @NonNull Context context) {
        this(context, null);
    }

    public SkinConstraintLayout(final @NonNull Context context, final @Nullable AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public SkinConstraintLayout(final @NonNull Context context, final @Nullable AttributeSet attrs,
                                final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        initAttributes(context, attrs);
    }

//    @SuppressLint("ClickableViewAccessibility")
//    @Override
//    public boolean onTouchEvent(final MotionEvent event) {
//        if (event.getAction() == MotionEvent.ACTION_DOWN && mIsClickChangeColor) {
//            setAlpha(0.6f);
//            return true;
//        } else if (event.getAction() == MotionEvent.ACTION_UP && mIsClickChangeColor) {
//            // 手指松开反馈点击事件并恢复控件原有的透明度
//            setAlpha(1.0f);
//            this.playSoundEffect(SoundEffectConstants.CLICK);
//            callOnClick();
//            return true;
//        } else if (event.getAction() == MotionEvent.ACTION_CANCEL && mIsClickChangeColor) {
//            // 手指划出点击区域恢复控件原有的透明度
//            setAlpha(1.0f);
//            return true;
//        }
//        return super.onTouchEvent(event);
//    }

    /**
     * 初始化属性
     *
     * @param context 上下文
     * @param attrs   属性
     */
    private void initAttributes(final Context context, final AttributeSet attrs) {
        final TypedArray typedArray = context.obtainStyledAttributes(attrs,
                R.styleable.SkinClickChangeColor);
        mIsClickChangeColor = typedArray.getBoolean(
                R.styleable.SkinClickChangeColor_click_color_change, false);
        typedArray.recycle();
    }


    @Override
    protected void drawableStateChanged() {
        super.drawableStateChanged();
        boolean isPressed = isPressed();
        boolean isFocused = isFocused();
        if (isFocused && isPressed) {
            //MFC 获取焦点并且按压态 使用XML 配置MFC Background
        } else if (isFocused) {
            //MFC 获取焦点并且按压态  使用XML 配置MFC Background
        } else if (isPressed && mIsClickChangeColor) {
            // 触屏按压态
            setAlpha(0.6f);
        } else if (mIsClickChangeColor) {
            // 恢复默认透明度
            setAlpha(1.0f);
        }
    }


    /**
     * 设置是否点击改变颜色
     * @param isClickChangeColor 是否点击改变颜色
     */
    public void setIsClickChangeColor(boolean isClickChangeColor) {
        mIsClickChangeColor = isClickChangeColor;
    }
}
