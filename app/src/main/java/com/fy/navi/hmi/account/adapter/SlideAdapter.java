package com.fy.navi.hmi.account.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.hmi.R;

import java.util.List;

public class SlideAdapter extends RecyclerView.Adapter<SlideAdapter.SlideViewHolder> {
    private final List<SlideItem> slideItems;

    public SlideAdapter(List<SlideItem> slideItems) {
        this.slideItems = slideItems;
    }

    @NonNull
    @Override
    public SlideViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.item_slide_page, parent, false);
        return new SlideViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull SlideViewHolder holder, int position) {
        SlideItem item = slideItems.get(position);
        holder.imageView.setImageResource(item.getImageResId());
        holder.textView.setText(item.getText());
        holder.textView2.setText(item.getDes());
    }

    @Override
    public int getItemCount() {
        return slideItems.size();
    }

    static class SlideViewHolder extends RecyclerView.ViewHolder {
        ImageView imageView;
        TextView textView;
        TextView textView2;

        public SlideViewHolder(@NonNull View itemView) {
            super(itemView);
            imageView = itemView.findViewById(R.id.car_connect_bg);
            textView = itemView.findViewById(R.id.car_connect_title);
            textView2 = itemView.findViewById(R.id.car_connect_title_des);
        }
    }
}
