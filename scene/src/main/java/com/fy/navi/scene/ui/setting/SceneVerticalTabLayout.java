package com.fy.navi.scene.ui.setting;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.view.View;
import android.widget.LinearLayout;

import com.fy.navi.scene.R;
import com.fy.navi.ui.view.SkinTabLayout;

public class SceneVerticalTabLayout extends SkinTabLayout {

    private boolean isMarginBottom;

    public SceneVerticalTabLayout(Context context) {
        super(context);
    }

    public SceneVerticalTabLayout(Context context, AttributeSet attrs) {
        super(context, attrs);
        init(context, attrs);
    }

    public SceneVerticalTabLayout(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init(context, attrs);
    }

    private void init(Context context, AttributeSet attrs) {
        TypedArray a = context.obtainStyledAttributes(attrs, R.styleable.SceneVerticalTabLayout);
        isMarginBottom = a.getBoolean(R.styleable.SceneVerticalTabLayout_isMarginBottom, false);
        a.recycle();
    }

    @Override
    protected void onLayout(boolean changed, int l, int t, int r, int b) {
        super.onLayout(changed, l, t, r, b);
        View tabStrip = getChildAt(0);
        if (tabStrip instanceof LinearLayout) {
            LinearLayout stripLayout = (LinearLayout) tabStrip;
            stripLayout.setOrientation(LinearLayout.VERTICAL);
            int childCount = stripLayout.getChildCount();
            // 调整所有tab的布局参数
            for (int i = 0; i < childCount; i++) {
                View tabView = stripLayout.getChildAt(i);
                LinearLayout.LayoutParams params = (LinearLayout.LayoutParams) tabView.getLayoutParams();
                params.width = LinearLayout.LayoutParams.MATCH_PARENT;
                params.height = LinearLayout.LayoutParams.WRAP_CONTENT;
                params.weight = 0;
                if (isMarginBottom) {
                    params.bottomMargin = 0;
                } else {
                    params.bottomMargin = 28;
                }
                tabView.setLayoutParams(params);
            }
        }
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
        View tabStrip = getChildAt(0);
        if (tabStrip instanceof LinearLayout) {
            ((LinearLayout) tabStrip).setOrientation(LinearLayout.VERTICAL);
        }
    }
} 