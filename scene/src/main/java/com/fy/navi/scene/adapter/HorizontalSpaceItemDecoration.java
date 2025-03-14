package com.fy.navi.scene.adapter;

import android.content.Context;
import android.graphics.Rect;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

public class HorizontalSpaceItemDecoration extends RecyclerView.ItemDecoration {

    private final int space;

    public HorizontalSpaceItemDecoration(int space) {
        this.space = space;
    }

    @Override
    public void getItemOffsets(Rect outRect, View view, RecyclerView parent, RecyclerView.State state) {
        outRect.left = space;
        outRect.right = space;

        // 如果是第一个 item，则不需要左边距；如果是最后一个 item，则不需要右边距
        int position = parent.getChildAdapterPosition(view);
        if (position == 0) {
            outRect.left = 0;
        } else if (position == parent.getAdapter().getItemCount() - 1) {
            outRect.right = 0;
        }
    }
}