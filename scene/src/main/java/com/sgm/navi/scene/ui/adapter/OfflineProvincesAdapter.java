package com.sgm.navi.scene.ui.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.AppCompatImageView;
import androidx.appcompat.widget.AppCompatTextView;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.sgm.navi.scene.R;
import com.sgm.navi.service.define.mapdata.CityDataInfo;
import com.sgm.navi.service.define.mapdata.ProvDataInfo;

import java.util.ArrayList;
import java.util.List;

/**
 * @author CaiYufei
 * @version \$Revision1.0\$
 * Date: 2025/2/20
 * Description: [省份数据适配器]
 */
public class OfflineProvincesAdapter extends RecyclerView.Adapter<OfflineProvincesAdapter.LimitProvincesViewHolder> {
    private final ArrayList<LimitProvinceBean> mData = new ArrayList<>();
    private OfflineCitiesAdapter.ItemClickListener mListener;
    private final Context mContext;

    public OfflineProvincesAdapter(final Context context, final ArrayList<ProvDataInfo> data) {
        this.mContext = context;
        for (ProvDataInfo provDataInfo : data) {
            this.mData.add(new LimitProvinceBean(provDataInfo));
        }
    }

    /**
     * 设置省份数据
     * @param data 省份数据
     */
    public void setData(final ArrayList<ProvDataInfo> data) {
        this.mData.clear();
        for (ProvDataInfo provDataInfo : data) {
            this.mData.add(new LimitProvinceBean(provDataInfo));
        }
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public LimitProvincesViewHolder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final View view = LayoutInflater.from(mContext).inflate(R.layout.item_limit_provinces, parent, false);
        return new LimitProvincesViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull final LimitProvincesViewHolder holder, final int position) {
        holder.mTvTitle.setText(mData.get(position).getProvince().getName());
        List<CityDataInfo> cities = mData.get(position).getProvince().getCityInfoList();
        final OfflineCitiesAdapter offlineCitiesAdapter = new OfflineCitiesAdapter(mContext, new ArrayList<>());
        if (cities == null || cities.isEmpty()) {
            final CityDataInfo cityDataInfo = new CityDataInfo();
            cityDataInfo.setName(mData.get(position).getProvince().getName());
            cityDataInfo.setAdcode(mData.get(position).getProvince().getAdcode());
            cities = new ArrayList<>();
            cities.add(cityDataInfo);
        }
        offlineCitiesAdapter.setData(cities);
        offlineCitiesAdapter.setListener(mListener);
        holder.mRecyclerView.setLayoutManager(new GridLayoutManager(mContext, 3));
        holder.mRecyclerView.setAdapter(offlineCitiesAdapter);


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

    public void setListener(final OfflineCitiesAdapter.ItemClickListener listener) {
        mListener = listener;
    }

    public static class LimitProvincesViewHolder extends RecyclerView.ViewHolder {
        private final AppCompatTextView mTvTitle;
        private final AppCompatImageView mIvContract;
        private final RecyclerView mRecyclerView;

        public LimitProvincesViewHolder(@NonNull final View itemView) {
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