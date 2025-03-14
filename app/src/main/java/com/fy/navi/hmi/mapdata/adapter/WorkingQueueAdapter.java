package com.fy.navi.hmi.mapdata.adapter;

import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.ItemCityDownloadingDataBinding;
import com.fy.navi.hmi.utils.StringUtils;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.CityDownLoadInfo;

import java.util.ArrayList;

public class WorkingQueueAdapter extends RecyclerView.Adapter<WorkingQueueAdapter.Holder> {
    private static final String TAG = WorkingQueueAdapter.class.getSimpleName();
    private ArrayList<CityDataInfo> cityDataInfoList;
    private OnItemClickListener itemClickListener;

    public WorkingQueueAdapter() {
    }

    public void setData(ArrayList<CityDataInfo> list) {
        cityDataInfoList = list;
        notifyDataSetChanged();
    }

    public void setItemClickListener(OnItemClickListener itemClickListener) {
        this.itemClickListener = itemClickListener;
    }

    @Override
    public Holder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        ItemCityDownloadingDataBinding cityDataBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.item_city_downloading_data, parent, false);
        return new Holder(cityDataBinding);
    }

    @Override
    public int getItemCount() {
        if (cityDataInfoList == null) {
            return 0;
        }
        return cityDataInfoList.size();
    }

    @Override
    public void onBindViewHolder(@NonNull Holder holder, int position) {
        holder.cityDataBinding.setModel(cityDataInfoList.get(position));
        //城市名称
        String cityName = cityDataInfoList.get(position).name;
        holder.cityDataBinding.itemCityName.setText(cityName);
        //城市下载状态
        CityDownLoadInfo downloadItem = cityDataInfoList.get(position).downLoadInfo;
        holder.cityDataBinding.itemStatusTip.setText(downloadItem.statusTip);
        //城市数据包大小
        String sizeString = StringUtils.formatSize(downloadItem.nFullZipSize);
        holder.cityDataBinding.itemCityData.setText(sizeString);

        //按钮操作
        holder.cityDataBinding.itemStatusTip.setOnClickListener(v -> {
            if (itemClickListener != null) {
                itemClickListener.onItemSuspendClick(position);
            }
        });
    }

    public static class Holder extends RecyclerView.ViewHolder {
        public ItemCityDownloadingDataBinding cityDataBinding;

        public Holder(ItemCityDownloadingDataBinding cityDataBinding) {
            super(cityDataBinding.getRoot());
            this.cityDataBinding = cityDataBinding;
            cityDataBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        void onItemSuspendClick(int index);
    }
}

