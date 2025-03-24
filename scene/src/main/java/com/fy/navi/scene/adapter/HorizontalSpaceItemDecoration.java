package com.fy.navi.scene.adapter;

import android.graphics.Rect;
import android.view.View;

import androidx.recyclerview.widget.RecyclerView;

public class HorizontalSpaceItemDecoration extends RecyclerView.ItemDecoration {

    private final int mSpace;

    public HorizontalSpaceItemDecoration(final int space) {
        this.mSpace = space;
    }

    @Override
    public void getItemOffsets(final Rect outRect, final View view, final RecyclerView parent, final RecyclerView.State state) {
        outRect.left = mSpace;
        outRect.right = mSpace;

        // 如果是第一个 item，则不需要左边距；如果是最后一个 item，则不需要右边距
        final int position = parent.getChildAdapterPosition(view);
        if (position == 0) {
            outRect.left = 0;
        } else if (position == parent.getAdapter().getItemCount() - 1) {
            outRect.right = 0;
        }
    }
}