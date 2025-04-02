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
 * @author LiuChang
 * @version  \$Revision.1.0\$
 * Date: 2025/2/26
 * Description: [限行主页城市选择适配器]
 */
public class LimitDriverCitiesAdapter extends RecyclerView.Adapter<LimitDriverCitiesAdapter.LimitDriverCitiesViewHolder> {
    private List<String> mData = new ArrayList<>();
    private ItemClickListener mListener;
    private int mSelectedPosition = -1;
    private final Context mContext;

    public LimitDriverCitiesAdapter(final Context context, final List<String> data) {
        this.mContext = context;
        this.mData = data;
        if(this.mData != null && !this.mData.isEmpty()) {
            mSelectedPosition = 0;
        }
    }

    /**
     * 设置数据
     * @param data 设置参数
     */
    public void setData(final List<String> data) {
        this.mData.clear();
        this.mData = data;
        if(this.mData != null && !this.mData.isEmpty()) {
            mSelectedPosition = 0;
            notifyItemRangeChanged(0, mData.size());
        }
    }

    @NonNull
    @Override
    public LimitDriverCitiesViewHolder onCreateViewHolder(final @NonNull ViewGroup parent, final int viewType) {
        final View view = LayoutInflater.from(mContext).inflate(R.layout.item_limit_driver_cities, parent, false);
        return new LimitDriverCitiesViewHolder(view);
    }

    @Override
    public void onBindViewHolder(final @NonNull LimitDriverCitiesViewHolder holder, final int position) {
        holder.mTvTitle.setText(mData.get(position));
        if (position == mSelectedPosition) {
            holder.mTvTitle.setTextColor(mContext.getColor(R.color.main_map_limit_current)); // 选中态为蓝色
        } else {
            holder.mTvTitle.setTextColor(mContext.getColor(R.color.main_map_limit_current_city)); // 未选中态为黑色或其他默认颜色
        }
        holder.mTvTitle.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(final View v) {
                if (mSelectedPosition != -1) {
                    notifyItemChanged(mSelectedPosition);
                }
                mSelectedPosition = position;
                notifyItemChanged(position);
                if (mListener != null) {
                    mListener.onClick(position);
                }
            }
        });
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    public static class LimitDriverCitiesViewHolder extends RecyclerView.ViewHolder {
        private final AppCompatTextView mTvTitle;

        public LimitDriverCitiesViewHolder(final @NonNull View itemView) {
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
         * @param position 点击位置下标
         */
        void onClick(int position);
    }
}