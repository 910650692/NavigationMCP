package com.fy.navi.hmi.limit;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.AppCompatImageView;
import androidx.appcompat.widget.AppCompatTextView;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.hmi.R;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;

import java.util.ArrayList;
import java.util.List;

/**
 * @author LiuChang
 * @version  \$Revision.1.0\$
 * Date: 2025/2/20
 * Description: [城市选择适配器]
 */
public class LimitProvincesAdapter extends RecyclerView.Adapter<LimitProvincesAdapter.LimitProvincesViewHolder> {
    private final ArrayList<LimitProvinceBean> mData = new ArrayList<>();
    private LimitCitiesAdapter.ItemClickListener mListener;
    private final Context mContext;

    public LimitProvincesAdapter(final Context context, final ArrayList<ProvDataInfo> data) {
        this.mContext = context;
        for (ProvDataInfo provDataInfo : data) {
            this.mData.add(new LimitProvinceBean(provDataInfo));
        }
    }

    /**
     * 设置数据
     * @param data 设置参数
     */
    public void setData(final ArrayList<ProvDataInfo> data) {
        this.mData.clear();
        for (ProvDataInfo provDataInfo : data) {
            this.mData.add(new LimitProvinceBean(provDataInfo));
        }
        notifyItemRangeChanged(0, mData.size());
    }

    @NonNull
    @Override
    public LimitProvincesViewHolder onCreateViewHolder(final @NonNull ViewGroup parent, final int viewType) {
        final View view = LayoutInflater.from(mContext).inflate(R.layout.item_limit_provinces, parent, false);
        return new LimitProvincesViewHolder(view);
    }

    @Override
    public void onBindViewHolder(final @NonNull LimitProvincesViewHolder holder, final int position) {
        holder.mTvTitle.setText(mData.get(position).getProvince().getName());
        List<CityDataInfo> cities = mData.get(position).getProvince().getCityInfoList();
        final LimitCitiesAdapter limitCitiesAdapter = new LimitCitiesAdapter(mContext, new ArrayList<>());
        if (cities == null || cities.isEmpty()) {
            final CityDataInfo cityDataInfo = new CityDataInfo();
            cityDataInfo.setName(mData.get(position).getProvince().getName());
            cityDataInfo.setAdcode(mData.get(position).getProvince().getAdcode());
            cities = new ArrayList<>();
            cities.add(cityDataInfo);
        }
        limitCitiesAdapter.setData(cities);
        limitCitiesAdapter.setListener(mListener);
        holder.mRecyclerView.setLayoutManager(new GridLayoutManager(mContext, 3));
        holder.mRecyclerView.setAdapter(limitCitiesAdapter);


        holder.mIvContract.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(final View v) {
                if (mData.get(position).isShowCities()) {
                    mData.get(position).setShowCities(false);
                    holder.mIvContract.setImageResource(R.drawable.img_limit_under);
                    holder.mRecyclerView.setVisibility(View.GONE);
                } else {
                    mData.get(position).setShowCities(true);
                    holder.mIvContract.setImageResource(R.drawable.img_limit_up);
                    holder.mRecyclerView.setVisibility(View.VISIBLE);
                }
            }
        });
    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    public void setListener(final LimitCitiesAdapter.ItemClickListener listener) {
        mListener = listener;
    }

    public static class LimitProvincesViewHolder extends RecyclerView.ViewHolder {
        private final AppCompatTextView mTvTitle;
        private final AppCompatImageView mIvContract;
        private final RecyclerView mRecyclerView;

        public LimitProvincesViewHolder(final @NonNull View itemView) {
            super(itemView);
            mTvTitle = itemView.findViewById(R.id.tv_title);
            mIvContract = itemView.findViewById(R.id.iv_contract);
            mRecyclerView = itemView.findViewById(R.id.recycler_view);
        }
    }

    public static class LimitProvinceBean {
        private ProvDataInfo mProvince;
        private boolean mShowCities = true;

        public LimitProvinceBean() {
        }

        public LimitProvinceBean(final ProvDataInfo province) {
            this.mProvince = province;
        }

        public ProvDataInfo getProvince() {
            return mProvince;
        }

        public void setProvince(final ProvDataInfo province) {
            this.mProvince = province;
        }

        public boolean isShowCities() {
            return mShowCities;
        }

        public void setShowCities(final boolean showCities) {
            this.mShowCities = showCities;
        }
    }
}