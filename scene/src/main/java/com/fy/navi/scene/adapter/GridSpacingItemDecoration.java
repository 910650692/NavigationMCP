package com.fy.navi.scene.adapter;

import android.content.Context;
import android.graphics.Rect;
import android.view.View;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

public class GridSpacingItemDecoration extends RecyclerView.ItemDecoration {

    private final int spanCount; // 列数
    private final int spacing;   // 上下间距 (dp)
    private final int horizontalSpacing; // 左右间距 (dp)
    private final boolean includeEdge; // 是否包括边缘
    private Context context;

    public GridSpacingItemDecoration(Context context,int spanCount, int spacing, int horizontalSpacing, boolean includeEdge) {
        this.spanCount = spanCount;
        this.spacing = spacing;
        this.horizontalSpacing = horizontalSpacing;
        this.includeEdge = includeEdge;
        this.context = context;
    }

    @Override
    public void getItemOffsets(@NonNull Rect outRect, @NonNull View view, @NonNull RecyclerView parent, @NonNull RecyclerView.State state) {
        int position = parent.getChildAdapterPosition(view); // item位置
        int column = position % spanCount; // 计算列号

        int spacingPx = dpToPx(spacing);
        int horizontalSpacingPx = dpToPx(horizontalSpacing);

        if (includeEdge) {
            // 包括边缘的情况
            outRect.left = horizontalSpacingPx - column * horizontalSpacingPx / spanCount; // 左边距
            outRect.right = (column + 1) * horizontalSpacingPx / spanCount; // 右边距

            if (position < spanCount) { // 第一行
                outRect.top = spacingPx;
            }
            outRect.bottom = spacingPx; // 底部间距
        } else {
            // 不包括边缘的情况
            outRect.left = column * horizontalSpacingPx / spanCount; // 左边距
            outRect.right = horizontalSpacingPx - (column + 1) * horizontalSpacingPx / spanCount; // 右边距

            if (position >= spanCount) { // 非第一行
                outRect.top = spacingPx; // 上下间距
            }
        }
    }

    private int dpToPx(int dp) {
        float density = context.getResources().getDisplayMetrics().density;
        return Math.round(dp * density);
    }
}