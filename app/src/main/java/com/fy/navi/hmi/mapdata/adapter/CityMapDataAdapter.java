package com.fy.navi.hmi.mapdata.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.ItemCityDataBinding;
import com.fy.navi.hmi.utils.StringUtils;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.CityDownLoadInfo;

import java.util.ArrayList;

public class CityMapDataAdapter extends RecyclerView.Adapter<CityMapDataAdapter.Holder> {
    private static final String TAG = CityMapDataAdapter.class.getSimpleName();
    private Context context;
    private ArrayList<CityDataInfo> cityDataInfoList;
    private OnItemClickListener itemClickListener;

    public CityMapDataAdapter(Context context) {
        this.context = context;
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
        ItemCityDataBinding cityDataBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.item_city_data, parent, false);
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
        Logger.d(TAG, "onItemClick onBindViewHolder: ", GsonUtils.toJson(downloadItem));
        if (downloadItem.statusTip.equals("下载中")) {
            holder.cityDataBinding.itemDownloadStatus.setAlpha(1.0f);
            holder.cityDataBinding.itemStatusTip.setText(downloadItem.statusTip + downloadItem.percent);
        } else if (downloadItem.statusTip.equals("已下载")) {
            holder.cityDataBinding.itemDownloadStatus.setAlpha(0.3f);
            holder.cityDataBinding.itemStatusTip.setText(downloadItem.statusTip);
        } else {
            holder.cityDataBinding.itemDownloadStatus.setAlpha(1.0f);
            holder.cityDataBinding.itemStatusTip.setText(downloadItem.statusTip);
        }
        //城市数据包大小
        String sizeString = StringUtils.formatSize(downloadItem.nFullZipSize);
        holder.cityDataBinding.itemCityData.setText(sizeString); // sizeString

        //按钮操作
        holder.cityDataBinding.itemDownloadStatus.setOnClickListener(v -> {
            if (itemClickListener != null) {
                ArrayList<Integer> cityAdcodes = new ArrayList<>();
                cityAdcodes.add(cityDataInfoList.get(position).adcode);
                itemClickListener.onItemClick(cityAdcodes);
            }
        });
    }

    public static class Holder extends RecyclerView.ViewHolder {
        public ItemCityDataBinding cityDataBinding;

        public Holder(ItemCityDataBinding cityDataBinding) {
            super(cityDataBinding.getRoot());
            this.cityDataBinding = cityDataBinding;
            cityDataBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        void onItemClick(ArrayList<Integer> cityAdCodes);
    }
}

