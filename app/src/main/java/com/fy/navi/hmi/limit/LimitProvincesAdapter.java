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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Author: LiuChang
 * Date: 2025/2/20
 * Description: [城市选择适配器]
 */
public class LimitProvincesAdapter extends RecyclerView.Adapter<LimitProvincesAdapter.LimitProvincesViewHolder> {
    private ArrayList<LimitProvinceBean> data = new ArrayList<>();
    private LimitCitiesAdapter.ItemClickListener mListener;
    private Context mContext;

    public LimitProvincesAdapter(Context context,ArrayList<ProvDataInfo> data) {
        this.mContext = context;
        this.data.clear();
        for (ProvDataInfo provDataInfo : data) {
            this.data.add(new LimitProvinceBean(provDataInfo));
        }
    }

    public void setData(ArrayList<ProvDataInfo> data) {
        this.data.clear();
        for (ProvDataInfo provDataInfo : data) {
            this.data.add(new LimitProvinceBean(provDataInfo));
        }
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public LimitProvincesViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(mContext).inflate(R.layout.item_limit_provinces, parent, false);
        return new LimitProvincesViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull LimitProvincesViewHolder holder, int position) {
        holder.tvTitle.setText(data.get(position).getProvince().name);
        List<CityDataInfo> cities = data.get(position).getProvince().cityInfoList;
        LimitCitiesAdapter limitCitiesAdapter = new LimitCitiesAdapter(mContext, new ArrayList<>());
        if (cities == null || cities.isEmpty()) {
            CityDataInfo cityDataInfo = new CityDataInfo();
            cityDataInfo.name = data.get(position).getProvince().name;
            cityDataInfo.adcode = data.get(position).getProvince().adcode;
            cities = new ArrayList<>();
            cities.add(cityDataInfo);
        }
        limitCitiesAdapter.setData(cities);
        limitCitiesAdapter.setListener(mListener);
        holder.recyclerView.setLayoutManager(new GridLayoutManager(mContext, 3));
        holder.recyclerView.setAdapter(limitCitiesAdapter);


        holder.ivContract.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (data.get(position).isShowCities()) {
                    data.get(position).setShowCities(false);
                    holder.ivContract.setImageResource(R.drawable.img_limit_under);
                    holder.recyclerView.setVisibility(View.GONE);
                } else {
                    data.get(position).setShowCities(true);
                    holder.ivContract.setImageResource(R.drawable.img_limit_up);
                    holder.recyclerView.setVisibility(View.VISIBLE);
                }
            }
        });
    }

    @Override
    public int getItemCount() {
        return data.size();
    }

    public void setListener(LimitCitiesAdapter.ItemClickListener listener) {
        mListener = listener;
    }

    public static class LimitProvincesViewHolder extends RecyclerView.ViewHolder {
        AppCompatTextView tvTitle;
        AppCompatImageView ivContract;
        RecyclerView recyclerView;

        public LimitProvincesViewHolder(@NonNull View itemView) {
            super(itemView);
            tvTitle = itemView.findViewById(R.id.tv_title);
            ivContract = itemView.findViewById(R.id.iv_contract);
            recyclerView = itemView.findViewById(R.id.recycler_view);
        }
    }

    public static class LimitProvinceBean {
        private ProvDataInfo province;
        private boolean showCities = true;

        public LimitProvinceBean() {
        }

        public LimitProvinceBean(ProvDataInfo province) {
            this.province = province;
        }

        public ProvDataInfo getProvince() {
            return province;
        }

        public void setProvince(ProvDataInfo province) {
            this.province = province;
        }

        public boolean isShowCities() {
            return showCities;
        }

        public void setShowCities(boolean showCities) {
            this.showCities = showCities;
        }
    }
}