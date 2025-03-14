package com.fy.navi.hmi.limit;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.AppCompatTextView;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.hmi.R;

import java.util.ArrayList;
import java.util.List;

/**
 * Author: LiuChang
 * Date: 2025/2/26
 * Description: [限行主页城市选择适配器]
 */
public class LimitDriverCitiesAdapter extends RecyclerView.Adapter<LimitDriverCitiesAdapter.LimitDriverCitiesViewHolder> {
    private List<String> data = new ArrayList<>();
    private ItemClickListener mListener;
    private int selectedPosition = -1;
    private Context mContext;

    public LimitDriverCitiesAdapter(Context context, List<String> data) {
        this.mContext = context;
        this.data.clear();
        this.data = data;
        if(this.data != null && !this.data.isEmpty()) {
            selectedPosition = 0;
        }
    }

    public void setData(List<String> data) {
        this.data.clear();
        this.data = data;
        if(this.data != null && !this.data.isEmpty()) {
            selectedPosition = 0;
        }
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public LimitDriverCitiesViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(mContext).inflate(R.layout.item_limit_driver_cities, parent, false);
        return new LimitDriverCitiesViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull LimitDriverCitiesViewHolder holder, int position) {
        holder.tvTitle.setText(data.get(position));
        if (position == selectedPosition) {
            holder.tvTitle.setTextColor(mContext.getColor(R.color.dialog_use_reminder_terms_service_color)); // 选中态为蓝色
        } else {
            holder.tvTitle.setTextColor(mContext.getColor(R.color.main_map_limit_current_city)); // 未选中态为黑色或其他默认颜色
        }
        holder.tvTitle.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (selectedPosition != -1) {
                    notifyItemChanged(selectedPosition);
                }
                selectedPosition = position;
                notifyItemChanged(position);
                if (mListener != null) {
                    mListener.onClick(position);
                }
            }
        });
    }

    @Override
    public int getItemCount() {
        return data.size();
    }

    public static class LimitDriverCitiesViewHolder extends RecyclerView.ViewHolder {
        AppCompatTextView tvTitle;

        public LimitDriverCitiesViewHolder(@NonNull View itemView) {
            super(itemView);
            tvTitle = itemView.findViewById(R.id.tv_title);
        }
    }

    public void setListener(ItemClickListener listener) {
        mListener = listener;
    }

    public interface ItemClickListener {
        void onClick(int position);
    }
}