package com.sgm.navi.scene.adapter;

import android.content.Context;
import android.graphics.Rect;
import android.view.View;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

public class GridSpacingItemDecoration extends RecyclerView.ItemDecoration {

    private final int mSpanCount; // 列数
    private final int mSpacing;   // 上下间距 (dp)
    private final int mHorizontalSpacing; // 左右间距 (dp)
    private final boolean mIncludeEdge; // 是否包括边缘
    private Context mContext;

    public GridSpacingItemDecoration(@NonNull final Context context, final int spanCount, final int spacing,
                                     final int horizontalSpacing, final boolean includeEdge) {
        this.mSpanCount = spanCount;
        this.mSpacing = spacing;
        this.mHorizontalSpacing = horizontalSpacing;
        this.mIncludeEdge = includeEdge;
        this.mContext = context;
    }

    @Override
    public void getItemOffsets(@NonNull final Rect outRect, @NonNull final View view,
                               @NonNull final RecyclerView parent, @NonNull final RecyclerView.State state) {
        final int position = parent.getChildAdapterPosition(view); // item位置
        final int column = position % mSpanCount; // 计算列号

        final int spacingPx = dpToPx(mSpacing);
        final int horizontalSpacingPx = dpToPx(mHorizontalSpacing);

        if (mIncludeEdge) {
            // 包括边缘的情况
            outRect.left = horizontalSpacingPx - column * horizontalSpacingPx / mSpanCount; // 左边距
            outRect.right = (column + 1) * horizontalSpacingPx / mSpanCount; // 右边距

            if (position < mSpanCount) { // 第一行
                outRect.top = spacingPx;
            }
            outRect.bottom = spacingPx; // 底部间距
        } else {
            // 不包括边缘的情况
            outRect.left = column * horizontalSpacingPx / mSpanCount; // 左边距
            outRect.right = horizontalSpacingPx - (column + 1) * horizontalSpacingPx / mSpanCount; // 右边距

            if (position >= mSpanCount) { // 非第一行
                outRect.top = spacingPx; // 上下间距
            }
        }
    }

    /**
     * dp转px
     * @param dp dp值
     * @return px值
     */
    private int dpToPx(final int dp) {
        final float density = mContext.getResources().getDisplayMetrics().density;
        return Math.round(dp * density);
    }
}