package com.sgm.navi.hmi.account.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.sgm.navi.hmi.R;

import java.util.List;

public class SlideAdapter extends RecyclerView.Adapter<SlideAdapter.SlideViewHolder> {
    private final List<SlideItem> mSlideItems;

    public SlideAdapter(final List<SlideItem> slideItems) {
        this.mSlideItems = slideItems;
    }

    @NonNull
    @Override
    public SlideViewHolder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final View view = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.item_slide_page, parent, false);
        return new SlideViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull final SlideViewHolder holder, final int position) {
        final SlideItem item = mSlideItems.get(position);
        holder.mImageView.setImageResource(item.getImageResId());
        holder.mTextView.setText(item.getText());
        holder.mTextView2.setText(item.getDes());
    }

    @Override
    public int getItemCount() {
        return mSlideItems.size();
    }

    static class SlideViewHolder extends RecyclerView.ViewHolder {
        private final ImageView mImageView;
        private final TextView mTextView;
        private final TextView mTextView2;

        public SlideViewHolder(@NonNull final View itemView) {
            super(itemView);
            mImageView = itemView.findViewById(R.id.car_connect_bg);
            mTextView = itemView.findViewById(R.id.car_connect_title);
            mTextView2 = itemView.findViewById(R.id.car_connect_title_des);
        }
    }
}
