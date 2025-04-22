package com.fy.navi.scene.adapter;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.ViewGroup;
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

    /**
     * 更新列表
     * @param gasInfos 列表数据
     * */
    public void setGasStationList(final List<GasStationInfo> gasInfos) {
        if (ConvertUtils.isEmpty(gasInfos)) {
            return;
        }
        mGasStationInfos.clear();
        mGasStationInfos.addAll(gasInfos);
        notifyDataSetChanged();
    }

    @Override
    public Holder onCreateViewHolder(final ViewGroup parent, final int viewType) {
        final GasStationItemBinding gasStationItemBinding =
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
    public void onBindViewHolder(final Holder holder, @SuppressLint("RecyclerView")final int position) {
        String type = mGasStationInfos.get(position).getType();
        if ("B5生物柴油".equals(type)) {
            type = "B5";
        } else if ("0#车柴".equals(type)) {
            type = "0#";
        }
        holder.mGasStationItemBinding.poiGasOilType.setText(type);
        holder.mGasStationItemBinding.poiGasOilPrice.setText(mGasStationInfos.get(position).getPrice());
    }

    public class Holder extends RecyclerView.ViewHolder {
        private GasStationItemBinding mGasStationItemBinding;

        public Holder(final GasStationItemBinding gasStationItemBinding) {
            super(gasStationItemBinding.getRoot());
            this.mGasStationItemBinding = gasStationItemBinding;
            gasStationItemBinding.setHolder(this);
        }
    }
}