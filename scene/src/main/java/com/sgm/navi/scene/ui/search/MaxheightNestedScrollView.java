package com.sgm.navi.scene.ui.search;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.util.TypedValue;

import com.android.utils.ScreenUtils;
import com.sgm.navi.scene.R;
import com.sgm.navi.ui.view.SkinNestedScrollView;

public class MaxheightNestedScrollView extends SkinNestedScrollView {
    private int maxHeight;

    public MaxheightNestedScrollView(final Context context) {
        super(context);
    }

    public MaxheightNestedScrollView(final Context context, final AttributeSet attrs) {
        super(context, attrs);
        initialize(context, attrs);
    }

    public MaxheightNestedScrollView(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        initialize(context, attrs);
    }

    /**
     * 初始化
     * @param context context对象
     * @param attrs xml属性集合
     */
    private void initialize(final Context context, final AttributeSet attrs) {
        final TypedArray typedArray = context.obtainStyledAttributes(attrs, R.styleable.MaxHeightScrollView);
        maxHeight = typedArray.getDimensionPixelSize(R.styleable.MaxHeightScrollView_maxHeight, getMaxDefaultHeight());
        typedArray.recycle();
    }

    @Override
    protected void onMeasure(final int widthMeasureSpec, final int heightMeasureSpec) {
        int mode = MeasureSpec.getMode(heightMeasureSpec);
        int size = MeasureSpec.getSize(heightMeasureSpec);
        int heightMeasure = heightMeasureSpec;
        // 测量子视图以获取其期望的大小
        int childHeight = getChildAt(0).getMeasuredHeight();
        if (childHeight == 0) { // 如果还没有测量过子视图，则先测量一次
            final int childWidthMeasureSpec = MeasureSpec.makeMeasureSpec(
                    MeasureSpec.getSize(widthMeasureSpec), MeasureSpec.AT_MOST);
            final int childHeightMeasureSpec = MeasureSpec.makeMeasureSpec(
                    Integer.MAX_VALUE >> 2, MeasureSpec.AT_MOST);
            getChildAt(0).measure(childWidthMeasureSpec, childHeightMeasureSpec);
            childHeight = getChildAt(0).getMeasuredHeight();
        }

        if (mode == MeasureSpec.AT_MOST || mode == MeasureSpec.UNSPECIFIED) {
            // 如果是 AT_MOST 或者 UNSPECIFIED 模式
            if (childHeight < maxHeight) {
                // 当子视图的高度小于最大高度时，使用子视图的实际高度
                size = childHeight;
                mode = MeasureSpec.EXACTLY;
            } else {
                // 当子视图的高度大于或等于最大高度时，使用最大高度
                size = maxHeight;
                mode = MeasureSpec.EXACTLY;
            }
        }

        // 使用修改后的尺寸重新计算
        heightMeasure = MeasureSpec.makeMeasureSpec(size, mode);

        super.onMeasure(widthMeasureSpec, heightMeasure);
    }

    private int getMaxDefaultHeight() {
        // 这里可以设置默认的最大高度，例如 420dp 转换成像素值
        return ScreenUtils.Companion.getInstance().dp2px(420);
    }
}
