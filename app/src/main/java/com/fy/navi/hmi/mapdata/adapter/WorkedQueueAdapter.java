package com.fy.navi.hmi.mapdata.adapter;

import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.ItemCityDownloadedDataBinding;
import com.fy.navi.hmi.utils.StringUtils;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.CityDownLoadInfo;

import java.util.ArrayList;

public class WorkedQueueAdapter extends RecyclerView.Adapter<WorkedQueueAdapter.Holder> {
    private static final String TAG = WorkedQueueAdapter.class.getSimpleName();
    private ArrayList<CityDataInfo> cityDataInfoList;
    private OnItemClickListener itemClickListener;

    public WorkedQueueAdapter() {
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
        ItemCityDownloadedDataBinding cityDataBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.item_city_downloaded_data, parent, false);
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
        holder.cityDataBinding.itemCityData.setText(sizeString); // sizeString

        //按钮操作
        holder.cityDataBinding.itemBtnStatus.setOnClickListener(v -> {
            if (itemClickListener != null) {
                ArrayList<Integer> cityAdcodes = new ArrayList<>();
                cityAdcodes.add(cityDataInfoList.get(position).adcode);
                itemClickListener.onItemDeleteClick(cityAdcodes);
            }
        });
    }

    public static class Holder extends RecyclerView.ViewHolder {
        public ItemCityDownloadedDataBinding cityDataBinding;

        public Holder(ItemCityDownloadedDataBinding cityDataBinding) {
            super(cityDataBinding.getRoot());
            this.cityDataBinding = cityDataBinding;
            cityDataBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        void onItemDeleteClick(ArrayList<Integer> cityAdCodes);
    }
}

