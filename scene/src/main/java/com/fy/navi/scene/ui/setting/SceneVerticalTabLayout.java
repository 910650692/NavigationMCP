package com.fy.navi.scene.ui.setting;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.view.View;
import android.widget.LinearLayout;

import com.fy.navi.scene.R;
import com.fy.navi.ui.view.SkinTabLayout;

public class SceneVerticalTabLayout extends SkinTabLayout {

    private boolean mIsMarginBottom;

    public SceneVerticalTabLayout(final Context context) {
        super(context);
    }

    public SceneVerticalTabLayout(final Context context, final AttributeSet attrs) {
        super(context, attrs);
        init(context, attrs);
    }

    public SceneVerticalTabLayout(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init(context, attrs);
    }

    /**
     * 初始化
     * @param context 上下文
     * @param attrs 属性
     */
    private void init(final Context context, final AttributeSet attrs) {
        final TypedArray a = context.obtainStyledAttributes(attrs, R.styleable.SceneVerticalTabLayout);
        mIsMarginBottom = a.getBoolean(R.styleable.SceneVerticalTabLayout_isMarginBottom, false);
        a.recycle();
    }

    @Override
    protected void onLayout(final boolean changed, final int l, final int t, final int r, final int b) {
        super.onLayout(changed, l, t, r, b);
        final View tabStrip = getChildAt(0);
        if (tabStrip instanceof LinearLayout) {
            final LinearLayout stripLayout = (LinearLayout) tabStrip;
            stripLayout.setOrientation(LinearLayout.VERTICAL);
            final int childCount = stripLayout.getChildCount();
            // 调整所有tab的布局参数
            for (int i = 0; i < childCount; i++) {
                final View tabView = stripLayout.getChildAt(i);
                final LinearLayout.LayoutParams params = (LinearLayout.LayoutParams) tabView.getLayoutParams();
                params.width = LinearLayout.LayoutParams.MATCH_PARENT;
                params.height = LinearLayout.LayoutParams.WRAP_CONTENT;
                params.weight = 0;
                if (mIsMarginBottom) {
                    params.bottomMargin = 0;
                } else {
                    params.bottomMargin = 28;
                }
                tabView.setLayoutParams(params);
            }
        }
    }

    @Override
    protected void onMeasure(final int widthMeasureSpec, final int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
        final View tabStrip = getChildAt(0);
        if (tabStrip instanceof LinearLayout) {
            ((LinearLayout) tabStrip).setOrientation(LinearLayout.VERTICAL);
        }
    }
} 