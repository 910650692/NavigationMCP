package com.fy.navi.scene.ui.adapter;


import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.AppCompatTextView;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.scene.R;
import com.fy.navi.service.define.mapdata.CityDataInfo;

import java.util.ArrayList;
import java.util.List;

/**
 * Author: LiuChang
 * Date: 2025/2/20
 * Description: [限行城市选择适配器]
 */
public class OfflineCitiesAdapter extends RecyclerView.Adapter<OfflineCitiesAdapter.LimitCitiesViewHolder> {
    private List<CityDataInfo> data = new ArrayList<>();
    private ItemClickListener mListener;
    private Context mContext;

    public OfflineCitiesAdapter(Context context, List<CityDataInfo> data) {
        this.mContext = context;
        this.data.clear();
        this.data = data;
    }

    public void setData(List<CityDataInfo> data) {
        this.data.clear();
        this.data = data;
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public LimitCitiesViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(mContext).inflate(R.layout.item_limit_cities, parent, false);
        return new LimitCitiesViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull LimitCitiesViewHolder holder, int position) {
        holder.tvTitle.setText(data.get(position).name);
        holder.tvTitle.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (mListener != null) {
                    mListener.onClick(data.get(position).adcode);
                }
            }
        });
    }

    @Override
    public int getItemCount() {
        return data.size();
    }

    public static class LimitCitiesViewHolder extends RecyclerView.ViewHolder {
        AppCompatTextView tvTitle;

        public LimitCitiesViewHolder(@NonNull View itemView) {
            super(itemView);
            tvTitle = itemView.findViewById(R.id.tv_title);
        }
    }

    public void setListener(ItemClickListener listener) {
        mListener = listener;
    }

    public interface ItemClickListener {
        void onClick(int cityCode);
    }
}