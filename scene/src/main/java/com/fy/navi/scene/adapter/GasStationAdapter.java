package com.fy.navi.scene.adapter;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.GasStationItemBinding;
import com.fy.navi.service.define.search.GasStationInfo;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.List;

public class GasStationAdapter extends RecyclerView.Adapter<GasStationAdapter.Holder> {
    private List<GasStationInfo> mGasStationInfos;

    public GasStationAdapter() {
        mGasStationInfos = new ArrayList<>();
    }

    public void setGasStationList(List<GasStationInfo> gasInfos) {
        if (ConvertUtils.isEmpty(gasInfos)) {
            return;
        }
        mGasStationInfos.clear();
        mGasStationInfos.addAll(gasInfos);
        notifyDataSetChanged();
    }

    @Override
    public Holder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        GasStationItemBinding gasStationItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.gas_station_item, parent, false);
        return new Holder(gasStationItemBinding);
    }

    @Override
    public int getItemCount() {
        if (ConvertUtils.isEmpty(mGasStationInfos)) {
            return NumberUtils.NUM_0;
        }
        return mGasStationInfos.size();
    }

    @Override
    public void onBindViewHolder(@NonNull Holder holder, @SuppressLint("RecyclerView") int position) {
        holder.gasStationItemBinding.poiGasOilType.setText(mGasStationInfos.get(position).getType());
        holder.gasStationItemBinding.poiGasOilPrice.setText(mGasStationInfos.get(position).getPrice());
    }

    public class Holder extends RecyclerView.ViewHolder {
        public GasStationItemBinding gasStationItemBinding;

        public Holder(GasStationItemBinding gasStationItemBinding) {
            super(gasStationItemBinding.getRoot());
            this.gasStationItemBinding = gasStationItemBinding;
            gasStationItemBinding.setHolder(this);
        }
    }
}