package com.sgm.navi.scene.ui.adapter;


import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.AppCompatTextView;
import androidx.recyclerview.widget.RecyclerView;

import com.sgm.navi.scene.R;
import com.sgm.navi.service.define.mapdata.CityDataInfo;

import java.util.ArrayList;
import java.util.List;

/**
 * @author CaiYufei
 * @version \$Revision1.0\$
 * Date: 2025/2/20
 * Description: [限行城市选择适配器]
 */
public class OfflineSearchResultAdapter extends RecyclerView.Adapter<OfflineSearchResultAdapter.LimitCitiesViewHolder> {
    private List<CityDataInfo> mData = new ArrayList<>();
    private ItemClickListener mListener;
    private final Context mContext;

    public OfflineSearchResultAdapter(final Context context, final List<CityDataInfo> data) {
        this.mContext = context;
        this.mData = data;
    }

    /**
     * 设置离线搜索结果数据
     * @param data 离线结果数据
     */
    public void setData(final List<CityDataInfo> data) {
        this.mData.clear();
        this.mData = data;
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public LimitCitiesViewHolder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final View view = LayoutInflater.from(mContext).inflate(R.layout.item_limit_cities, parent, false);
        return new LimitCitiesViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull final LimitCitiesViewHolder holder, final int position) {
        holder.mTvTitle.setText(mData.get(position).getName());
        holder.mTvTitle.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(final View v) {
                if (mListener != null) {
                    mListener.onClick(mData.get(position).getAdcode());
                }
            }
        });
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    public static class LimitCitiesViewHolder extends RecyclerView.ViewHolder {
        private final AppCompatTextView mTvTitle;

        public LimitCitiesViewHolder(@NonNull final View itemView) {
            super(itemView);
            mTvTitle = itemView.findViewById(R.id.tv_title);
        }
    }

    public void setListener(final ItemClickListener listener) {
        mListener = listener;
    }

    public interface ItemClickListener {
        /**
         * 点击事件
         * @param cityCode 点击item的城市编码
         */
        void onClick(int cityCode);
    }
}