package com.fy.navi.hmi.limit;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.AppCompatTextView;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.hmi.R;
import com.fy.navi.service.define.mapdata.CityDataInfo;

import java.util.ArrayList;
import java.util.List;

/**
 * @author LiuChang
 * @version  \$Revision.1.0\$
 * Date: 2025/2/20
 * Description: [限行城市选择适配器]
 */
public class LimitCitiesAdapter extends RecyclerView.Adapter<LimitCitiesAdapter.LimitCitiesViewHolder> {
    private List<CityDataInfo> mDate = new ArrayList<>();
    private ItemClickListener mListener;
    private final Context mContext;

    public LimitCitiesAdapter(final Context context, final List<CityDataInfo> data) {
        this.mContext = context;
        this.mDate = data;
    }

    /**
     * 设置数据
     *
     * @param data 数据
     */
    public void setData(final List<CityDataInfo> data) {
        this.mDate.clear();
        this.mDate = data;
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public LimitCitiesViewHolder onCreateViewHolder(final @NonNull ViewGroup parent,final int viewType) {
        final View view = LayoutInflater.from(mContext).inflate(R.layout.item_limit_cities, parent, false);
        return new LimitCitiesViewHolder(view);
    }

    @Override
    public void onBindViewHolder(final @NonNull LimitCitiesViewHolder holder, final int position) {
        holder.mTitle.setText(mDate.get(position).getName());
        holder.mTitle.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(final View v) {
                if (mListener != null) {
                    mListener.onClick(String.valueOf(mDate.get(position).getAdcode()));
                }
            }
        });
    }

    @Override
    public int getItemCount() {
        return mDate.size();
    }

    public static class LimitCitiesViewHolder extends RecyclerView.ViewHolder {
        private final AppCompatTextView mTitle;

        public LimitCitiesViewHolder(final @NonNull View itemView) {
            super(itemView);
            mTitle = itemView.findViewById(R.id.tv_title);
        }
    }

    public void setListener(final ItemClickListener listener) {
        mListener = listener;
    }

    public interface ItemClickListener {
        /**
         * 城市点击事件
         * @param cityCode 城市id
         */
        void onClick(String cityCode);
    }
}