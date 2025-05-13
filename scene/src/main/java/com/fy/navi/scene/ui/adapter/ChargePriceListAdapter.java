package com.fy.navi.scene.ui.adapter;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.ChargePriceItemBinding;
import com.fy.navi.service.define.search.ChargePriceInfo;
import com.fy.navi.service.define.search.CostTime;

import java.util.ArrayList;
import java.util.List;

public class ChargePriceListAdapter extends RecyclerView.Adapter<ChargePriceListAdapter.priceHolder>{
    private ArrayList<CostTime> mChargePriceList = new ArrayList<>();
    @NonNull
    @Override
    public priceHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        ChargePriceItemBinding chargePriceItemBinding = DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.charge_price_item,parent,false);
        return new priceHolder(chargePriceItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull priceHolder holder, int position) {
        holder.chargePriceItemBinding.setCostTime(mChargePriceList.get(position));
    }

    // 更新列表
    public void notifyList(ArrayList<CostTime> list){
        mChargePriceList.addAll(list);
        notifyDataSetChanged();
    }

    @Override
    public int getItemCount() {
        if(mChargePriceList == null){
            return 0;
        }
        return mChargePriceList.size();
    }

    public static class priceHolder extends RecyclerView.ViewHolder{
        public ChargePriceItemBinding chargePriceItemBinding;
        public priceHolder(@NonNull ChargePriceItemBinding binding) {
            super(binding.getRoot());
            this.chargePriceItemBinding = binding;
        }
    }
}
